# nim 1.4.8
# hack asm


import os
import strutils
import options
import sugar
import sequtils
import tables


type SymbolTable = object
    table: Table[string, uint]
    variableCounter: uint


func initSymbolTable(): SymbolTable =
    let t = initTable[string, uint]()
    let c: uint = 16
    return SymbolTable(table: t, variableCounter: c)


type Instruction = enum
    AInstruction
    CInstruction


func normalize(line: string): string =
    ## delete comments and spaces
    var rline = line
    let cmt = rline.find("//")
    if cmt >= 0:
        rline = rline[0..cmt-1]
    rline = rline.strip()

    return rline


func machineFilename(asmFilename: string): string =
    let splitted = asmFilename.split(".")
    let machine = splitted[0..splitted.len-2] & "hack"
    return machine.join(".")


func instructionParser(line: string): Instruction =
    if line[0] == '@':
        return AInstruction
    return CInstruction


func aParser(line: string, symbolTable: var SymbolTable): string =
    ## Parse A instruction

    let valueString = line[1..len(line)-1]
    
    if valueString[0] == 'R':
        try:
            return "0" & valueString[1..len(valueString)-1].parseInt().toBin(15)
        except ValueError:
            discard
    
    case valueString
    of "SP":
        return "0" & 0.toBin(15)
    of "LCL":
        return "0" & 1.toBin(15)
    of "ARG":
        return "0" & 2.toBin(15)
    of "THIS":
        return "0" & 3.toBin(15)
    of "THAT":
        return "0" & 4.toBin(15)
    of "SCREEN":
        return "0" & 16384.toBin(15)
    of "KBD":
        return "0" & 24576.toBin(15)

    # look symbol table
    if symbolTable.table.hasKey(valueString):
        return "0" & symbolTable.table[valueString].int.toBin(15)
    
    try:
        return "0" & valueString.parseInt().toBin(15)
    except ValueError:  # 文字列
        symbolTable.table[valueString] = symbolTable.variableCounter
        symbolTable.variableCounter += 1
        return "0" & symbolTable.table[valueString].int.toBin(15)


func destCoder(dest: string): string =
    let d = ['A', 'D', 'M']

    return map(d, (x) => dest.contains(x))
            .map((x) => int(x).intToStr()).join()


func jumpCoder(jump: string): string =
    return case jump
    of "JGT":
        "001"
    of "JEQ":
        "010"
    of "JGE":
        "011"
    of "JLT":
        "100"
    of "JNE":
        "101"
    of "JLE":
        "110"
    of "JMP":
        "111"
    else:
        "000"


func compCoder(comp: string): string =
    let a = int(comp.contains('M')).intToStr()

    let c = case comp
    of "0":         "101010"
    of "1":         "111111"
    of "-1":        "111010"
    of "D":         "001100"
    of "A","M":     "110000"
    of "!D":        "001101"
    of "!A","!M":   "110001"
    of "-D":        "001111"
    of "-A","-M":   "110011"
    of "D+1":       "011111"
    of "A+1","M+1": "110111"
    of "D-1":       "001110"
    of "A-1","M-1": "110010"
    of "D+A","D+M": "000010"
    of "D-A","D-M": "010011"
    of "A-D","M-D": "000111"
    of "D&A","D&M": "000000"
    of "D|A","D|M": "010101"
    else:
        "000000"

    return a & c


func cParser(line: string): string =
    # Parse C instruction

    # Dest=Comp;Jump
    var comp, dest, jump: string
    comp = line
    
    # jump
    let j = comp.find(';')
    if j >= 0:
        jump = comp[j+1..len(comp)-1]
        comp = comp[0..j-1]

    # dest
    let d = comp.find('=')
    if d >= 0:
        dest = comp[0..d-1]
        comp = comp[d+1..len(comp)-1]

    return "111" & compCoder(comp) & destCoder(dest) & jumpCoder(jump)


func lineProcessor(line: string, symbolTable: var SymbolTable): Option[string] =
    let instruction: Instruction = instructionParser(line)
    case instruction
    of AInstruction:
        return some(line.aParser(symbolTable))
    of CInstruction:
        return some(line.cParser)


func createSymbolTable(allLines: seq[string]): (SymbolTable, seq[string]) =
    var symbolTable: SymbolTable = initSymbolTable()
    var i: uint = 0
    var newLines: seq[string] = @[]

    for line in allLines:
        if line.contains('('):
            let symbol = line.replace("(", "").replace(")", "")
            symbolTable.table[symbol] = i
            continue
        newLines.add(line)
        i += 1

    return (symbolTable, newLines)


proc main() =
    if os.paramCount() == 0:
        echo "ERROR: ファイルを指定してください"
        quit(-1)
    
    let filename: string = os.commandLineParams()[0]


    block:
        let fasm: File = open(filename, FileMode.fmRead)
        let fmachine: File = open(filename.machineFilename, FileMode.fmWrite)

        # read all lines
        let allLines: seq[string] = map(
            fasm.readAll().split('\n'),
            (x) => normalize(x)
        ).filter((x) => x.len > 0)
        
        # make symbol table
        var (symbolTable, newLines) = createSymbolTable(allLines)
        echo "===SYMBOL TABLE==="
        for k, v in symbolTable.table.pairs:
            echo k & ":\t" & $v

        # compile
        for line in newLines:
            let converted: Option[string] = lineProcessor(line, symbolTable)
            if converted.isNone:
                continue
            fmachine.writeLine(converted.get())

        defer:
            close(fasm)
            close(fmachine)


when isMainModule:
    main()
