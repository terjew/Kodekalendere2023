namespace Utilities

type Matrix = 
    {
        SizeX : int
        SizeY : int
        Data : string[]
    }

module Matrix =
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

    let equal matrix matrix2 = 
        matrix.SizeX = matrix2.SizeX &&
        matrix.SizeY = matrix2.SizeY &&
        matrix.Data = matrix2.Data
        
    let get matrix (x,y) =
        matrix.Data[y][x]

    let rows matrix =
        seq {
            for row in 0 .. matrix.SizeY - 1 do yield matrix.Data[row]
        }

    let row matrix y =
        matrix.Data[y]

    let allPos matrix =
        seq {
            for y in 0 .. matrix.SizeY - 1 do 
                for x in 0 .. matrix.SizeX - 1 do yield (x,y)
        }

    let map func matrix =

        let transformRow matrix y func =
            seq { 
                for x in 0 .. matrix.SizeX - 1 do yield (func matrix (x,y) )
            } |> Array.ofSeq |> System.String

        let transformedRows = seq {
            for y in 0 .. matrix.SizeY - 1 do yield transformRow matrix y func
        }
        {
            SizeX = matrix.SizeX
            SizeY = matrix.SizeY
            Data = Array.ofSeq transformedRows
        }

    let find value matrix =
        matrix 
        |> allPos 
        |> Seq.find (fun pos -> value = get matrix pos)

    let neighborCoordsDiagonal matrix (x,y)  = 
        let minx = if x = 0 then x else x - 1
        let miny = if y = 0 then y else y - 1
        let maxx = if x = matrix.SizeX - 1 then x else x + 1
        let maxy = if y = matrix.SizeY - 1 then y else y + 1
        seq {
        for iy in miny .. maxy do
            for ix in minx .. maxx do
                if not (ix = x && iy = y) then yield (ix, iy)
        }

    let neighborsDiagonal matrix pos =
        neighborCoordsDiagonal matrix pos
        |> Seq.map (get matrix)

    let isInside matrix (x,y) =
        x >= 0 && x < matrix.SizeX && y >= 0 && y < matrix.SizeY

    let offsetPos (x,y) (xOffset,yOffset) =
        (x + xOffset, y + yOffset)

    let neighborInDirection pos dir =
        (dir, dir |> Direction.offset |> offsetPos pos)

    let neighborCoordsWithDirection matrix pos =
        Direction.cardinal 
        |> Seq.map (neighborInDirection pos)
        |> Map.ofSeq
        |> Map.filter (fun dir pos -> isInside matrix pos)

    let neighborsWithDirection matrix pos =
        neighborCoordsWithDirection matrix pos
        |> Map.map (fun dir pos -> get matrix pos)

    let withValueAt matrix (x,y) value =
        let updatedLine = matrix.Data[y] |> String.mapi(fun i char -> if i=x then value else char)
        let updatedArray = matrix.Data |> Array.mapi(fun i line -> if i=y then updatedLine else line)
        {
            SizeX = matrix.SizeX;
            SizeY = matrix.SizeY;
            Data = updatedArray
        }

    let print matrix =
        printfn "Matrix[%d,%d]" matrix.SizeX matrix.SizeY
        printfn "" 
        for y in 0 .. matrix.SizeY - 1 do
            printfn "%s" matrix.Data[y]
        printfn "" 
