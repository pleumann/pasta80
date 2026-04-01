; -------------------------------------------------------------------------
; --- Some Lua helper functions that extend sjasmplus for our purposes ----
; -------------------------------------------------------------------------

    LUA

function SegInfo(Name, Start, End, MaxLen, Where)
    Len = End - Start
    Start = Start % 65536
    End = (End - 1) % 65536

    if Len > 0 then
        Str = string.format("%-10s: %5d bytes ($%04X-$%04X) %-10s", Name, Len, Start, End, Where)
    else
        Str = string.format("%-10s:     0 bytes ($XXXX-$XXXX) %-10s", Name, Where)
    end

    if Len > MaxLen then
        print(Str.."  * Too large *")
    else
        print(Str)
    end
end

function OvrInfo(Number, Is128K)
    Page = sj.calc(string.format("OVR_%d_PAGE", Number))
    Start = sj.calc(string.format("OVR_%d_START", Number))
    End = sj.calc(string.format("OVR_%d_END", Number))

    if Is128K == 1 then
        SegInfo(string.format("Overlay %2d", Number), Start, End, 8192, string.format("in bank %2d", Page))
    else
        SegInfo(string.format("Overlay %2d", Number), Start, End, 8192, string.format("in page %2d", Page))
    end
end

local Breakpoints = {}

function AddBreakpoint(Cond)
    table.insert(Breakpoints, {sj.calc("$"), Cond})
end

function WriteBreakpoints(Name, Prefix, Format1, Format2, Suffix)
    File = assert(io.open(Name, "w"))
    File:write(Prefix)
    for _, Tuple in ipairs(Breakpoints) do
        if Tuple[2] == nil then
            File:write(string.format(Format1, Tuple[1]))
        else
            File:write(string.format(Format2, Tuple[1], Tuple[2]))
        end
    end
    File:write(Suffix)
    File:close()
end

function WriteAgonBreakpoints(Name)
    WriteBreakpoints(Name, "-d", " -b 0x4%04X", " -b 0x4%04X", "")
end

function WriteFuseBreakpoints(Name)
    WriteBreakpoints(Name, "--debugger-command 'del", "\nbr 0x%04X", "\nbr 0x%04X if %s", "'")
end

local Files = {}

local Statements = {}

function FileNumber(File)
    for I, F in ipairs(Files) do
        if F == File then
            return I - 1
        end
    end
    Files[#Files + 1] = File
    return #Files - 1
end

function AddLineInfo(File, Line, Column)
    table.insert(Statements, {sj.calc("$"), FileNumber(File), Line, Column})
end

function WriteLineInfo(Name)
    File = assert(io.open(Name, "w"))

    File:write("{\n")
    File:write("  \"files\": [\n")
    
    for I, F in ipairs(Files) do
        File:write(string.format("    \"%s\"", F))
        if I == #Files then
            File:write("\n")
        else
            File:write(",\n")
        end
    end
    
    File:write("  ],\n")
    File:write("  \"statements\": [\n")

    for I, S in ipairs(Statements) do
        File:write(string.format("    { \"addr\": \"0x%04X\", \"file\": %d, \"line\": %d, \"column\": %d }", S[1], S[2], S[3], S[4]))
        if I == #Statements then
            File:write("\n")
        else
            File:write(",\n")
        end
    end
    
    File:write("  ]\n")
    File:write("}\n")

    File:close()
end

    ENDLUA