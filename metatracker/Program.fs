open Un4seen.Bass

let sampleRate = 44100
let samplesPerMinute = sampleRate * 60
type SampleArray = int16[]
let makeSampleArray l = Array.create l (int16(0))

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

type ISampleProvider =
    abstract member RetrieveSamples : int -> SampleArray -> int

type SamplePlayer(path) =
    let samples = getSampleDataFromFile(path)
    interface ISampleProvider with
        member this.RetrieveSamples offset (outSamples:SampleArray) =
            let available = max 0 (min (samples.Length-offset) outSamples.Length)
            Array.blit samples offset outSamples 0 available
            available

type Instrument( source : ISampleProvider ) =
    let mutable consumers = Map.empty<(System.Guid * int),int>
    let cursors = System.Collections.Generic.List<int>()
    member this.GetOrCreateHandle( guid, depth ) =
        if consumers.ContainsKey( guid, depth ) then
            consumers.Item( guid, depth )
        else
            cursors.Add(0)
            cursors.Count
    member this.GetSamples( handle, outSamples ) =
        cursors.[handle-1] <- cursors.[handle-1] + (source.RetrieveSamples cursors.[handle-1] outSamples)

type Library = System.Collections.Generic.List<Instrument option>

type Track() =
    let guid = System.Guid.NewGuid()
    let mutable data = Array.create 16 0
    let mutable volume = 0.5
    let mutable instrument : Instrument option = None
    let mutable handle = 0
    member this.Guid with get () = guid
    member this.Volume with get () = volume and set (value) = volume <- value
    member this.Inst with get () = instrument and set (value) = instrument <- value
    member this.Handle with get () = handle and set (value) = handle <- value
    member this.GetLine( line ) = data.[line]
    member this.SetLine( line, value ) = data.[line] <- value
    
type Pattern(library:Library) =
    let tracks = [| new Track(); new Track(); new Track() |]
    let mutable lines = 16
    let mutable lineCursor = 0
    let mutable bpm = 135
    let mutable lpb = 4
    let samplesPerLine = int( ceil( float(samplesPerMinute) / float(bpm*lpb) ) )
    let samples = makeSampleArray (lines * samplesPerLine)
    let mutable cachedAmount = 0;
    let fillCache( neededSize ) =
        while cachedAmount < neededSize do
            for t in tracks do
                let temp = makeSampleArray samplesPerLine
                if t.GetLine(lineCursor) > 0 then
                    t.Inst <- Some library.[t.GetLine(lineCursor)].Value
                    t.Handle <- t.Inst.Value.GetOrCreateHandle(t.Guid,0) // TODO: Depth? and how?
                if t.Inst.IsSome then
                    t.Inst.Value.GetSamples( t.Handle, temp )
                    Array.iteri (fun i v -> samples.[lineCursor*samplesPerLine+i] <- samples.[lineCursor*samplesPerLine+i] + (int16(round(float(v)*t.Volume)))) temp
            cachedAmount <- cachedAmount + samplesPerLine
            lineCursor <- lineCursor + 1
    do
        for i=0 to 3 do tracks.[0].SetLine(4*i,1)   // kick
        for i=0 to 3 do tracks.[0].SetLine(4*i+2,3) // hat
        for i=0 to 1 do tracks.[1].SetLine(8*i+4,2) // snare
        // tracks.[2].SetLine(3,4) // For experts only...
    member this.BPM with get () = bpm and set (value) = bpm <- value
    member this.LPB with get () = lpb and set (value) = lpb <- value
    member this.Lines with get () = lines and set (value) = lines <- value
    member this.LengthInSamples = lines * samplesPerLine
    interface ISampleProvider with
        member this.RetrieveSamples offset (outSamples : SampleArray) =
            let neededSize = offset+outSamples.Length
            if neededSize > cachedAmount then fillCache( neededSize )
            let available = max 0 (min (cachedAmount-offset) outSamples.Length)
            Array.blit samples offset outSamples 0 available
            available

let writePatternToWAV (pattern:Pattern) path =
    let writer = new Misc.WaveWriter(path,1,44100,16,true)
    let outSamples = makeSampleArray pattern.LengthInSamples
    (pattern :> ISampleProvider).RetrieveSamples 0 outSamples |> ignore
    writer.Write(outSamples,outSamples.Length*2) // *2... KAAAHHHHNNNNNNNN!
    writer.Close()

[<EntryPoint>]
let main argv =
    printfn "%A" argv
    if not (initBass()) then raise (new System.Exception("Bass failed to load?"))
    
    let library = new Library()
    let p = new Pattern(library)
    library.Add(None)
    library.Add(Some (new Instrument(new SamplePlayer(".\\sounds\\BT3A0D3.wav"))))
    library.Add(Some (new Instrument(new SamplePlayer(".\\sounds\\ST0TASA.wav"))))
    library.Add(Some (new Instrument(new SamplePlayer(".\\sounds\\HHCDA.wav"))))
    library.Add(Some (new Instrument(p)))
    
    writePatternToWAV p "out.wav"
    
    freeBass()
    0 // return an integer exit code
