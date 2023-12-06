namespace Utilities

module Matrix =
    type Matrix = 
        {
            SizeX : int
            SizeY : int
            Data : string[]
        }

    let create sizeX sizeY (initial : char) = 
        {
            SizeX = sizeX;
            SizeY = sizeY;
            Data = Array.create sizeY (Array.create sizeX initial |> System.String)
        }

    let fromStringArray (data : string[]) =
        let sizeX = data[0].Length
        let sizeY = data.Length
        {
            SizeX = sizeX;
            SizeY = sizeY;
            Data = data
        }

    let fromString (str : string) =
        str |> String.splitWithAny "\r\n" |> fromStringArray

    let fromStrings strings = 
        let data = Seq.toArray strings
        fromStringArray data

    let get x y matrix =
        matrix.Data[y][x]

    let rows matrix =
        seq {
            for row in 0 .. matrix.SizeY - 1 do yield matrix.Data[row]
        }

    let row y matrix =
        matrix.Data[y]

    let neighborCoords x y matrix = 
        let minx = if x = 0 then x else x - 1
        let miny = if y = 0 then y else y - 1
        let maxx = if x = matrix.SizeX - 1 then x else x + 1
        let maxy = if y = matrix.SizeY - 1 then y else y + 1
        seq {
        for iy in miny .. maxy do
            for ix in minx .. maxx do
                if not (ix = x && iy = y) then yield (ix, iy)
        }

    let neighbors x y matrix =
        neighborCoords x y matrix
        |> Seq.map (fun (x,y) -> get x y matrix)

    let modifiedAt x y value matrix =
        let updatedLine = matrix.Data[y] |> String.mapi(fun i char -> if i=x then value else char)
        let updatedArray = matrix.Data |> Array.mapi(fun i line -> if i=y then updatedLine else line)
        {
            SizeX = matrix.SizeX;
            SizeY = matrix.SizeY;
            Data = updatedArray
        }

    let print matrix =
        printfn "Matrix[%d,%d]" matrix.SizeX matrix.SizeY
        for y in 0 .. matrix.SizeY - 1 do
            printfn "%s" matrix.Data[y]
