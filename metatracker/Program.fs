[<EntryPoint>]
let main argv = 
    printfn "%A" argv
    let kick = new NAudio.Wave.AudioFileReader(".\\sounds\\BT3A0D3.wav")
    let snare = new NAudio.Wave.AudioFileReader(".\\sounds\\ST0TASA.wav")
    let l = int32(max kick.Length snare.Length)
    let kickSamples = Array.create l (float32(0))
    let snareSamples = Array.create l (float32(0))
    ignore (kick.Read( kickSamples, 0, (int32(kick.Length)) ))
    ignore (snare.Read( snareSamples, 0, (int32(snare.Length)) ))

    let outSamples = Array.create l (float32(0))
    ignore (Array.mapi2 (fun i a b -> outSamples.[i] <- float32(0.5)*(a+b)) kickSamples snareSamples)
    let writer = new NAudio.Wave.WaveFileWriter( "out.wav", (new NAudio.Wave.WaveFormat()) )
    writer.WriteSamples(outSamples, 0, outSamples.Length)
    writer.Dispose()
    0 // return an integer exit code
