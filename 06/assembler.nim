# nim 1.4.8
# hack asm


import os
import strutils
import options
import sugar
import sequtils




type Command = enum
    ACommand
    CCommand
    # TODO: Symbol



func commandParser(line: string): Command =
    if line[0] == '@':
        return ACommand
    return CCommand


func aParser(line: string): string =
    ## Parse A command
    let value: int = line[1..line.len()-1].parseInt()
    return "0" & value.toBin(15)


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
    # Parse C command

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




func machineFilename(asmFilename: string): string =
    let splitted = asmFilename.split(".")
    let machine = splitted[0..splitted.len-2] & "hack"
    return machine.join(".")


func lineProcessor(line: string): Option[string] =
    # ignore comments and blank line
    if line.contains("//") or line.strip().len == 0:
        return none(string)
    
    let command: Command = commandParser(line)
    case command
    of ACommand:
        return some(line.aParser)
    of CCommand:
        return some(line.cParser)



proc main() =
    if os.paramCount() == 0:
        echo "ERROR: ファイルを指定してください"
        quit(-1)
    
    let filename: string = os.commandLineParams()[0]


    block:
        let fasm: File = open(filename, FileMode.fmRead)
        let fmachine: File = open(filename.machineFilename, FileMode.fmWrite)
        
        while true:
            try:
                let line = fasm.readLine()
                let converted: Option[string] = lineProcessor(line)
                if converted.isNone:
                    continue
                fmachine.writeLine(converted.get())
            except EOFError:
                break
        defer:
            close(fasm)
            close(fmachine)


when isMainModule:
    main()

