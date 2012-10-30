open Un4seen.Bass

let sampleRate = 44100
let samplesPerMinute = sampleRate * 60
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

type Track() =
    let mutable data = Array.create 16 0
    let mutable volume = 0.5
    let mutable playable : IPlayable = (new NullPlayable():>IPlayable)
    member this.Volume with get () = volume and set (value) = volume <- value
    member this.Playable with get () = playable and set (value) = playable <- value
    member this.GetLine( line ) = data.[line]
    member this.SetLine( line, value ) = data.[line] <- value

type Pattern(library:Library) =
    let tracks = [| new Track(); new Track() |]
    let mutable lines = 16
    let mutable bpm = 135
    let mutable lpb = 4
    let mutable lineCursor = 0
    let samplesPerLine = int( ceil( float(samplesPerMinute) / float(bpm*lpb) ) )
    let lineCache = makeSampleArray samplesPerLine
    let mutable cachedAmount = 0;
    let fillCache() =
        Array.fill lineCache 0 samplesPerLine (int16(0))
        for t in tracks do
            let temp = makeSampleArray samplesPerLine
            if t.GetLine(lineCursor) > 0 then
                t.Playable <- library.[t.GetLine(lineCursor)]
                t.Playable.NoteOn()
            t.Playable.GetSamples(temp)
            Array.iteri (fun i v -> lineCache.[i] <- lineCache.[i] + (int16(round(float(v)*t.Volume)))) temp
        lineCursor <- lineCursor + 1
        cachedAmount <- samplesPerLine
    do
        for i=0 to 3 do tracks.[0].SetLine(4*i,1)   // kick
        for i=0 to 3 do tracks.[0].SetLine(4*i+2,3) // hat
        for i=0 to 1 do tracks.[1].SetLine(8*i+4,2) // snare
    member this.BPM with get () = bpm and set (value) = bpm <- value
    member this.LPB with get () = lpb and set (value) = lpb <- value
    member this.Lines with get () = lines and set (value) = lines <- value
    member this.LengthInSamples = lines * samplesPerLine
    interface IPlayable with
        member this.NoteOn() =
            lineCursor <- 0
            fillCache()
        member this.GetSamples( outSamples : SampleArray ) =
            if cachedAmount = 0 then fillCache()
            let mutable samplesWanted = outSamples.Length
            let mutable samplesWritten = 0
            while samplesWanted > 0 do
                let available = min cachedAmount samplesWanted
                Array.blit lineCache (samplesPerLine - cachedAmount) outSamples samplesWritten available
                samplesWritten <- samplesWritten + available
                cachedAmount <- cachedAmount - available
                samplesWanted <- samplesWanted - available
                if (cachedAmount = 0 && samplesWanted > 0) then fillCache()

let writePatternToWAV (pattern:Pattern) path =
    let writer = new Misc.WaveWriter(path,1,44100,16,true)
    let mutable samplesToWrite = pattern.LengthInSamples
    while samplesToWrite > 0 do
        let outSamples = makeSampleArray (min 512 samplesToWrite)
        (pattern :> IPlayable).GetSamples( outSamples )
        writer.Write(outSamples,outSamples.Length*2) // *2... KAAAHHHHNNNNNNNN!
        if samplesToWrite >= 512 then samplesToWrite <- samplesToWrite - 512 else samplesToWrite <- 0
    writer.Close()

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
    
    writePatternToWAV p "out.wav"
    
    freeBass()
    0 // return an integer exit code
