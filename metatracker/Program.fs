open Un4seen.Bass

type SampleArray = int16[]
let makeSampleArray l = Array.create l (int16(0))
type IPlayable =
    abstract member NoteOn : unit -> unit
    abstract member GetSamples : SampleArray -> unit
type NullPlayable() =
    interface IPlayable with
        member this.NoteOn()=()
        member this.GetSamples(_)=()
type Library = System.Collections.Generic.List<IPlayable>

let initBass() = Bass.BASS_Init(-1,44100,BASSInit.BASS_DEVICE_DEFAULT,nativeint(0))
let freeBass() = Bass.BASS_Free() |> ignore

let getSampleDataFromFile(path:string) =
    let stream = Bass.BASS_StreamCreateFile(path,int64(0),int64(0),BASSFlag.BASS_STREAM_DECODE)
    let i = Bass.BASS_ChannelGetInfo(stream) // Useful, not strictly necessary...
    let l = int32(Bass.BASS_ChannelGetLength(stream))
    let s = makeSampleArray l
    (Bass.BASS_ChannelGetData(stream,s,l)) |> ignore
    Bass.BASS_StreamFree(stream) |> ignore
    s

type SamplePlayer(path) =
    let samples = getSampleDataFromFile(path)
    let mutable cursor = 0
    interface IPlayable with
        member this.NoteOn() = cursor <- 0
        member this.GetSamples( outSamples : SampleArray ) =
            if cursor < samples.Length then
                let available = min (samples.Length-cursor) outSamples.Length
                Array.blit samples cursor outSamples 0 available
                cursor <- cursor + available

type Pattern(library:Library) =
    let mutable lines =[|1;0;3;0;2;0;3;0;1;0;3;0;2;0;3;0|]
    let mutable bpm = 135
    let mutable lpb = 4
    let mutable cursor = 0
    let mutable lineCursor = 0
    let mutable playable : IPlayable = (new NullPlayable():>IPlayable)
    let mutable l = 44100 * (60*lines.Length) / (bpm*lpb) // 60 sec/min
    let samplesPerLine = l / lines.Length
    let lineCache = makeSampleArray samplesPerLine
    let mutable cacheReady = false;
    let fillCache() =
        cursor <- 0
        if lines.[lineCursor] > 0 then
            playable <- library.[lines.[lineCursor]]
            playable.NoteOn()
        playable.GetSamples(lineCache)
        lineCursor <- lineCursor + 1
        cacheReady <- true
    member this.BPM with get () = bpm and set (value) = bpm <- value
    member this.LPB with get () = lpb and set (value) = lpb <- value
    member this.Lines
        with get() = lines.Length
        and set(value) = lines <- Array.create value 0 // Oh my, data loss...
    interface IPlayable with
        member this.NoteOn() =
            cursor <- 0
            lineCursor <- 0
            fillCache()
        member this.GetSamples( outSamples : SampleArray ) =
            if not cacheReady then fillCache()
            let available = min (samplesPerLine-cursor) outSamples.Length
            Array.blit lineCache cursor outSamples 0 available
            if available = outSamples.Length then
                cursor <- cursor + available
            else // There's a gnarly assumption here that outSamples.length will never be bigger than samplesPerLine. (beware/fix)
                fillCache()
                let want = outSamples.Length - available
                Array.blit lineCache cursor outSamples available want
                cursor <- want

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    if not (initBass()) then raise (new System.Exception("Bass failed to load?"))
    
    let library = new Library()
    let p = new Pattern(library)
    library.Add(p)
    library.Add(new SamplePlayer(".\\sounds\\BT3A0D3.wav"))
    library.Add(new SamplePlayer(".\\sounds\\ST0TASA.wav"))
    library.Add(new SamplePlayer(".\\sounds\\HHCDA.wav"))
    
    let l = 44100 * (60*p.Lines) / (p.BPM*p.LPB)
    let outSamples = makeSampleArray l

    let mutable allDone = false
    let mutable cursor = 0
    while not allDone do
        let available = min (l - cursor) 512
        if available > 0 then
            let a = makeSampleArray available
            (p:>IPlayable).GetSamples(a)
            Array.blit a 0 outSamples cursor available
            cursor <- cursor + available
        else
            allDone <- true
    
    let writer = new Misc.WaveWriter("out.wav",1,44100,16,true)
    writer.Write(outSamples,l*2)
    writer.Close()
    freeBass()
    0 // return an integer exit code
