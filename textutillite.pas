// Textutil verze 09.08.2000
//@001 2011.12.27 Noah Silva = Changes for FPC
//@002 2011.12.29 Noah Silva = Remove Windows Dependencies
{$mode Delphi}       //@001+
unit textutillite;
//@001= Noah Silva = Changes for CLI use in FreePascal
interface
{$DEFINE NOFORMS}
uses SysUtils,
     {$ifdef TEXTGRAF}
   //  Graphics,      @002-
     {$endif}
     {$ifndef NOFORMS}
  //   Controls, Forms,
     {$endif}
     {$IFDEF WIN32} //@002+
     Windows,
     {$ENDIF} //@002+
     Classes;

function Subst(const s,find,replace: AnsiString): AnsiString;
function CharSubst(const S: String; Find,Replace: Char): String;
procedure ParseGeom(s: AnsiString; var l,t,w,h: integer);
function TitleCase(const s: AnsiString): AnsiString;
function BreakApart(BaseString: AnsiString; const BreakString: AnsiString; StringList: TStrings): TStrings;
function PutTogether1(StrLst: TStrings; const BreakString: AnsiString): AnsiString; // 1-pass
function PutTogether(StrLst: TStrings; const BreakString: AnsiString): AnsiString;  // faster, but 2-pass...
function StripComment(const s: AnsiString): AnsiString;
function sltSpc(const s: AnsiString): AnsiString; {skip lead&trail spaces}
function InsideQuote(const s: AnsiString): AnsiString;
procedure reinils(ls: TStrings);
function StrDup(src: PChar): PChar;
function StrDup_L(src: PChar; var len: integer): PChar;
procedure CountColRows(data: PChar; sep: char; var ccnt,rcnt: integer; lines: TList);
procedure FmtAddr(res: PChar; p: Pointer);
function WildMatch(const str, pattern: AnsiString): Boolean;
function MakeAppName(const name,defPath: string): string;
function GetFilterIndex(const filter,filename: AnsiString): integer; // 1-based index... 0=not found...
function IcIndexOf(Strings: TStrings; const Item: string): integer; // ignore-case IndexOf ... (AnsiCompareText)
procedure SListAddFmt(Strings: TStrings; const Fmt: string; const Args: array of const);

/////Intel 32-bit Assembler routines:
///Mostly ignoring #0 termination character (that's why they're fast)...
///Instead user must pass them pointer to end or size of the string...
///only {*}: (Pend=nil --> search #0)

//like strscan:
function FastScan(str: PChar; fi: char; maxsize: integer): PChar;
function FastScan2(str: PChar; fi: char; Pend: PChar): PChar;
//replace one char by another:
procedure FastCharSubst(str: PChar; orisep, newsep: char; Pend: PChar); {*}
//like strstr:       FastPos is slower than Pos!
function FastPos(str: PChar; find: PChar; filen: integer; strsize: integer): PChar;
//like strlen:
function FastLen(str: PChar): integer; assembler;
//Find end-of-line (#10, or #13 before #10) or nil, if #0 reached before...
function FastEOL1(str: PChar): PChar; // #0 first --> nil
function FastEOL(str: PChar): PChar;  // #0 first --> @(#0)
//compare 2 strings  (strncmp)
function FastComp(s1, s2: PChar; len: integer): Boolean;
function FastCompUntil(s1,s2: PChar; term: char): integer; // Return logical (s1-s2)
//like   memset(str,0,len)
procedure FastClear(str: Pointer; len: integer); assembler;
//like   memcpy(dest,src,len)
procedure FastCopy(dest, src: PChar; len: integer);
//copy until char.
function FastCopyUntil(dest, src: PChar; term: char): integer;
//count "sep" chars in a text
function FastCharCount(str: PChar; sep: char; Pend: PChar): integer; {*}
// find n-th "sep" char in str
function FastColumn(str: PChar; sep: char; cnt: integer; Pend: PChar): PChar;
function FastColumn2(str: PChar; sep: char; cnt: integer; Pend: PChar; var collen: integer): PChar;
function FastGetColumn(str: PChar; sep: char; cnt: integer; Pend: PChar): string;
//Friendly interface: GetColumn in string, LGetColumn in line (until FastEOL)
//GetColumn ignores #10...
function GetColumn(const data: string; sep: char; cnt: integer): string;
function LGetColumn(const line: string; sep: char; cnt: integer): string;

// alcomp ... calls AnsiStrLIComp       no-more !  now alComp calls AnsiCmp below...
// licomp ... calls StrLIComp
//function alComp(const s1,s2: string; len: integer): Boolean;
function liComp(const s1,s2: string; len: integer): Boolean;
function ICPos(const substr: AnsiString; const s: AnsiString): Integer; // Case insensitive Pos function...
// SysUtils replacements:
//function AnsiCompareText(const s1,s2: string): integer;
function AnsiUpperCase(const s1: string): string;
function AnsiLowerCase(const s1: string): string;
// Lo-level:
//function AnsiUp(a: char): char; register;
//function AnsiLo(a: char): char; register;
//procedure AnsiUpStr(pc: PChar; len: integer); register;
//procedure AnsiLoStr(pc: PChar; len: integer); register;
//function AnsiCmp(p1,p2: PChar; len: integer): integer; register;

// Extension compare:
function extComp(const filename,ext: string): Boolean;
function extCompEx(const filename: string; const exts: Array of string): integer;

function ExtractToken(var line: string; sep: char): string;
function pExtractToken(var line: PChar; sep: char): string;
function ExtractToken4(var line: string; seps4: PChar): string; // seps4=^array[0..3] of char; (for ex. seps4:=' '#9#10#13; )
function ExtractLine(var text: PChar): string;

function ExtractArg(var line: string): string; // "some arg" blabla    ~ arg blabla (Command-line arguments...)

function ExtractSectionNames(const inf: string; names: TStrings): integer;

function FindInfSection(const inf,section: string): PChar;
function ExtractInfSection(const inf,section: string): string;
procedure DeleteInfSection(var inf: string; const section: string);
procedure ReplaceInfSection(var inf: string; const name,data: string);
procedure MergeInf(var dest: string; const src: string; whole_sections: Boolean);

function GetInfValue(const section,name: string): string;
procedure SetInfValue(var section: string; const name,value: string); // deletes name, if value empty...
procedure SetInfValue2(var section: string; const name,value: string);// puts "name=", if value empty... 
procedure DeleteInfValue(var section: string; const name: string);

type TInfValueRec=record
       name,value: string;
       sec,ptr,prev: PChar;
     end;
// Inf value enumeration: (while res=0 ...)
function FindFirstInfValue(const section: string; var rec: TInfValueRec): integer;
function FindNextInfValue(const section: string; var rec: TInfValueRec): integer;

function ExtractSections(const inf: string; IniList: TStrings): integer;

// Object interface to ini-functions declared above:
type
  TIniStringList=class(TStringList)
  protected
    OpenSecName,OpenSecData: string;
    FClean,SecDirty: Boolean;
 //   procedure OpenSection(const SecName: string);
 //   procedure CloseSection(Store: Boolean);
    function GetSectionName(Index: integer): string;
  //  function GetSectionData(Index: integer): string;
    procedure Changed; override;
  //  function GetClean: Boolean;
  public
    ObjOrder: Boolean;
    constructor Create;
    procedure Assign(Source: TPersistent); override;
  //  property Clean: Boolean read GetClean;
    // Ini-File initialization:
   // procedure LoadIni(const IniFile: string);
  //  procedure StoreIni(const IniFile: string);
  //  function  GetIni: string;
    //procedure SetIni(const IniData: string);
  //  procedure MergeIni(const IniData: string; WholeSections: Boolean);
  //  procedure MergeIniList(MergeList: TStringList; WholeSections: Boolean);
    // Sections:
 //   function  SectionIndex(SecName: string): integer;
 //   function  GetSection(const SecName: string): string;
 //   procedure ReplaceSection(const SecName,SecData: string);
  //  procedure DeleteSection(const SecName: string);
    function  AddSection(const SecName,SecData: string): integer;
  //  function  RenameSection(const OldName,NewName: string): Boolean; // True=section order changed!
    property  SectionName[Index: integer]: string read GetSectionName;
//    property  SectionData[Index: integer]: string read GetSectionData;
    procedure GetSectionNames(DstList: TStrings; ObjSort: Boolean);
    // Values:
 //   function  GetValue(const SecName,ValName: string): string;
 //   procedure SetValue(const SecName,ValName,ValData: string);
 //   procedure DeleteValue(const SecName,ValName: string);
  end;

function ExtractQuoted(var p: PChar): string; // moves pointer after closing ", inside ("")->(")      29/18 times faster than AnsiExtractQuotedStr...

procedure CheckAppend(var str: string; ch: Char);
procedure AddSlash(var path: string);
function  StripSlash(const path: string): string;
procedure SkipBlank(var p: PChar);
function  pSkipBlank(p: PChar): PChar; // is better than SkipBlank <-- no need of stack variable
procedure SkipEOL(var p: PChar);
function  pSkipEOL(p: PChar): PChar;
function  SkipLine(p: PChar): PChar; // = pSkipEOL(FastEOL(p))

function StrLoadFile(const name: string): string;
function StrLoadFileEx(const name: string; existing: Boolean): string;
procedure StrStoreFile(const name,data: string);
{$ifndef MEDICUS}
function GetResData(const resname: string): string;
function GetResDataEx(Inst: THandle; const resname: string): string;
{$endif not MEDICUS}

// Like strdup:
function MemDup(src: Pointer; size: integer): Pointer;

procedure DataDump(const Filename: string; data: PChar; len: integer; silent: Boolean);

procedure BitSet(pbitarr: PChar; bit: smallint); assembler;
procedure BitReset(pbitarr: PChar; bit: smallint); assembler;
function BitTest(pbitarr: PChar; bit: smallint): Boolean; assembler;

{!!this can raise ETextError:}
function UntilNext(const s: AnsiString; var p: integer): AnsiString;

type TArgV=array[0..{?vari}7] of PChar; // & last is NIL ! item [-1] is ArrayAllocSize!
     PArgV=^TArgV;
     //PPChar=^PChar;   //@001-

function AllocArray(n_items: integer): PArgV; assembler;
function ReAllocArray(av: PArgV; n_add: integer): PPChar; assembler;
//procedure FreeArray(av: PArgV; itToo: Boolean); assembler; // can free all items inside
                                                           //items by StrDispose...
                                                           //array by FreeMem...
function ArrayAllocCount(av: PArgV): integer; assembler;
function ArrayCount(av: PArgV): integer; assembler;

type TLedType=(ledNum,ledScroll,ledCaps);
//function GetLedState(led: TLedType): Boolean;
//function SetLedState(led: TLedType; newState: Boolean): Boolean; // returns previous state...
//procedure SendKeys(const Keys: array of Word);

{color routines:}
{$ifdef TEXTGRAF}
function ParseColor(const s: AnsiString): TColor;
function XColor(clr: TColor): AnsiString;

{graphics:}
procedure DisplayTexts(cnv: TCanvas; ls: TStringList);
{$endif TEXTGRAF}

function iMax(a,b: integer): integer;
function iMin(a,b: integer): integer;

function ValPC(var pc: PChar; var Digit: integer): LongBool; register;

{$ifdef NOFORMS}
const MDlgOwner: THandle=0;

{$.define TXDIALOGS} // auto in NOFORMS mode...

{$endif}

{$ifdef TXDIALOGS}
type TAnnoyType=(tmInfo,tmWarn,tmYesNo,tmYesNoWarn,tmOkCancel,tmRetry,tmRetryErr,tmError);

procedure Warn(const msg: string);
function YesNo(const msg: string; isWarn: Boolean): Boolean;
function YesNoTop(const msg: string; isWarn: Boolean): Boolean;
function YesNoCancel(const msg: string; isWarn: Boolean): integer;
function OkCancel(const msg: string; isWarn: Boolean): Boolean;
function AbortRetryIgnore(const msg: string): integer;
function mdRetry(const msg: string): Boolean;
function mdRetry_Err(const msg: string): Boolean;
procedure Error(const msg: string);
procedure mdInfo(const msg: string);
procedure Annoy(const msg: string; skipList: TStrings; const item: string);
function AnnoyEx(const msg: string; dialogType: TAnnoyType; skipList: TStrings; const item: string; default: Boolean): Boolean;
procedure OutOfMem(failsize: integer);
{$endif TXDIALOGS}

{$ifdef TEXT_LISTUTIL}

{--------------------------------------------------------------------}
{ List util                                                          }
{--------------------------------------------------------------------}
procedure SaveFree(pobj: pointer);     // Usage:    SaveFree(@Object)
procedure SaveFreeMem(pptr: pointer);  // Usage: SaveFreeMem(@Pointer)

procedure ListAdd(var List: TList; Item: Pointer);
function ListAddSingle(var List: TList; Item: Pointer): Boolean;
function ListAddSingleFirst(var List: TList; Item: Pointer): Boolean;
procedure ListRemove(var List: TList; Item: Pointer);
procedure DestroyList(var List: TList); // with Objects...
procedure FreeListAndItems(var List: TList; Objects: Boolean); // with Objects~Pointers
procedure ListFreeByType(var List: TList; Cls: TClass);
procedure AssignList(dst,src: TList);

{$endif TEXT_LISTUTIL}

{.$ifdef DELPHI2}     //@001-
//const               //@001-
{.$else}              //@001-
resourcestring
  {.$endif}    //@001-
  sError='Error:';                     //@001=
  sWarning='Warning';                  //@001=
  sInformation='Information';          //@001=
  sConfirmation='Confirmation?';       //@001=

type ETextError=class(Exception);

implementation

procedure DataDump(const Filename: string; data: PChar; len: integer; silent: Boolean);
var F:File;
    wrsz: integer;
const cannotwrite='Nelze zapisovat do souboru ';
begin
  try
    AssignFile(F,Filename);
    ReWrite(F,1);
  except on E:Exception do begin
    {$ifdef TXDIALOGS}
    if not Silent then Warn(cannotwrite+Filename+#10#10+E.Message);
    {$endif}
    exit;
  end; end;
  try try
    BlockWrite(F,data^,len,wrsz);
    {$ifdef TXDIALOGS}
    if wrsz<len then 
      if not Silent then 
        Warn(cannotwrite+Filename+#10'('+IntToStr(len)+' != '+IntToStr(wrsz)+')');
    {$endif}
  except on E:Exception do begin
    {$ifdef TXDIALOGS}
    if not Silent then 
      Warn('Nastala chyba pﱋ z疳isu do souboru '+Filename+' : '#10+E.Message);
    {$endif}
  end; end;
  finally CloseFile(F); end;
end;

function WildMatch(const str, pattern: AnsiString): Boolean;
  function MatchPattern(element, patt: PChar): Boolean;
    {function IsPattWild(pat: PChar): Boolean;
    var t: integer;
    begin
      Result:=StrScan(pat,'*')<>nil;
      if not Result then Result:=StrScan(pat,'?')<>nil;
    end;}
  begin
    if {StrComp(patt,'*')=0} (patt^='*') and ((patt+1)^=#0) then Result:=True
    else if(element^=#0) and (patt^<>#0) then Result:=False
    else if(element^=#0) then Result:=True
    else begin
      case patt^ of
        '*': if MatchPattern(element,@patt[1]) then Result:=True
             else Result:=MatchPattern(@element[1],patt);
        '?': Result:=MatchPattern(@element[1],@patt[1]);
        else if element^=patt^ then Result:=MatchPattern(@element[1],@patt[1])
             else Result:=False;
      end;
    end;
  end;
begin
  Result:=MatchPattern(PChar(str),PChar(pattern));
end;{This code came from Lloyd's help-file, author: Sean Stanley in Tallahassee Florida}

function GetFilterIndex(const filter,filename: AnsiString): integer; // 1-based index... 0=not found...
var flt,flte,pext,psub,pdesc: PChar;
    wild: string;
    l: integer;
Label next;
begin
  Result:=0;
  if length(filter)=0 then exit;
  flt:=@filter[1]; flte:=flt+FastLen(flt);
  repeat
    inc(Result);
    // skip description:
    pext:=FastScan2(flt,'|',flte); if pext=nil then begin Result:=0; exit; end;
    inc(pext);
    // get next item (extract filter extensions):
    pdesc:=FastScan2(pext,'|',flte); if pdesc=nil then pdesc:=flte;
    if (pdesc-pext)=0 then begin flt:=pdesc+1; continue; end;
    repeat
      // Extract more filter sub-options ...|*.ex1;*.ex2|...
      psub:=FastScan2(pext,';',pdesc);
      if psub=nil then psub:=pdesc;
      l:=psub-pext;
      if l=0 then begin pext:=psub+1; continue; end;
      SetLength(wild,l); FastCopy(@wild[1],pext,l);
      if WildMatch(filename,wild) then exit; // FOUND!
      pext:=psub+1;
    until pext>=pdesc;
    flt:=pdesc+1;
  until flt>=flte;
  Result:=0;
end;

function FastScan(str: PChar; fi: char; maxsize: integer): PChar;
label found,not_found,put_res;
begin
  asm
    push edi; push esi; push ebx;
    mov ecx,maxsize
    mov edi,str
    or ecx,ecx
    jz not_found
    mov al,fi
    repne scasb
    or ecx,ecx
    jnz found            // ecx>0 --> nalezeno
    mov ah,[edi-1]
    cmp al,ah            // ecx=0 --> nenalezeno ~ posledni znak
    jne not_found
    dec edi              //matched last character, look if not walking back!
    cmp edi,str
    jae put_res
not_found:
    xor edi,edi
    jmp put_res
found:
    dec edi              //"repne scasb"  skonci o 1 za nalezem...
put_res:
    mov Result,edi
    pop ebx; pop esi; pop edi;
  end;
end;

function FastScan2(str: PChar; fi: char; Pend: PChar): PChar;
label found,not_found,put_res;
begin
  asm
    push edi; push esi; push ebx;
    mov edi,str
    mov ecx,Pend
    sub ecx,edi
    jbe not_found
    mov al,fi
    repne scasb
    or ecx,ecx
    jnz found            // ecx>0 --> nalezeno
    mov ah,[edi-1]
    cmp al,ah            // ecx=0 --> nenalezeno ~ posledni znak
    jne not_found
    dec edi              //matched last character, look if not walking back!
    cmp edi,str
    jae put_res
not_found:
    xor edi,edi
    jmp put_res
found:
    dec edi              //"repne scasb"  skonci o 1 za nalezem...
put_res:
    mov Result,edi
    pop ebx; pop esi; pop edi;
  end;
end;

procedure FastCharSubst(str: PChar; orisep, newsep: char; Pend: PChar);
Label fsc_out,fsc_have_end;
begin
  asm
    push edi; push esi; push ebx;
    mov edi,str
    mov ecx,Pend
    or edi,edi; jz fsc_out
    or ecx,ecx; jnz fsc_have_end
    mov edx,edi                  // search for string end:
    mov ecx,$FFFFFFFF
    xor eax,eax
    repne scasb
    mov ecx,edi
    mov edi,edx
fsc_have_end:
    sub ecx,edi
    mov al,orisep; mov ah, newsep;
@001:
    repne scasb
    or ecx,ecx; jz fsc_out
    mov [edi-1],ah
    jmp @001    
fsc_out:
    pop ebx; pop esi; pop edi;
  end;
end;

{$ifdef UNUSED}
function FastScan2_B(str: PChar; fi: char; Pend: PChar): PChar;
begin
  if Pend<str then Result:=nil else Result:=FastScan(str,fi,Pend-str);
end;
{$endif}

function FastPos(str: PChar; find: PChar; filen: integer; strsize: integer): PChar;
Label fast_pos_do,fast_pos,not_found_1,not_found,found,put_res,ch1cycle;
begin
  asm
    push edi;
    jmp fast_pos_do
    //sanity checks:
    mov edx,strsize
    or edx,edx
    jb not_found_1                             // strsize==0 --> return nil
    mov ecx,filen
    cmp ecx,1
    ja fast_pos
    jb not_found_1                             // filen==0 --> return nil
    pop edi;
  end;
    Result:=FastScan(str,find^,strsize);       // filen==1 --> use FastScan instead
    exit;
  asm
not_found_1:
    push esi; push ebx; // push these for poping at the end...
not_found:
    xor edi,edi
    jmp put_res

fast_pos:
    sub edx,ecx
    jb not_found_1                             // filen > strsize --> return nil

fast_pos_do: //FastPos itself:
    push esi; push ebx;
    mov edi,str                    //edi: str
    mov esi,find                   //esi: find
    mov ecx,strsize                //ecx: strsize
    mov al,[esi]                   //al : first char of find
ch1cycle:
    repne scasb                                                                               //repeat     if AL=[EDI] then done:=True; inc(EDI); dec(ECX); if(ECX==0) then done=True;     until done;
    or ecx,ecx
    jz not_found
    mov ebx,ecx                    //ebx: rest_size
    mov edx,edi                    //edx: saved pos after matched character
    mov ecx,filen
    dec edi
    mov esi,find
    repe cmpsb  //-------------------/
    mov edi,edx //... doesn't alter Z-flag ...
    mov ecx,ebx //...              /
    jne ch1cycle//    <-----------/
//found:
    //mov edi,edx    // moved above...
    dec edi
put_res:
    mov Result,edi
    pop ebx; pop esi; pop edi;
  end;
end;

function FastCharCount(str: PChar; sep: char; Pend: PChar): integer;
Label cycle,ret_err,ret_edx,have_end;
begin
  asm
    push edi; push ebx;
    mov edi,str
    mov ecx,Pend
    or edi,edi; jz ret_err
    or ecx,ecx; jnz have_end
    mov edx,edi
    xor eax,eax
    mov ecx,$FFFFFFFF
    repne scasb
    mov ecx,edi
    nop
    mov edi,edx
have_end:
    xor edx,edx
    sub ecx,edi
    jb ret_err
    jz ret_edx
    mov al,sep
cycle:
    repne scasb
    //cmp ecx,0; jbe ret_edx
    or ecx,ecx; jz ret_edx
    inc edx
    jmp cycle
ret_err:
    mov edx,$FFFFFFFF
ret_edx:
    mov Result,edx
    pop ebx; pop edi;
  end;
end;

function FastColumn(str: PChar; sep: char; cnt: integer; Pend: PChar): PChar;
Label cycle,done,not_found;
begin
  asm
    push edi; push ebx;
    mov edi,str                  //edi: string
    mov ecx,Pend; sub ecx,edi    //ecx: length
    jbe not_found   // ? len==0 ?
    mov edx,cnt                  //edx: cnt
    or edx,edx
    jz done         // cnt==0 --> return string
    mov al,sep                   //ah: sep
cycle:
    repne scasb
    or ecx,ecx //cmp ecx,0
    jbe not_found
    dec edx         // dec cnt
    jz done
    jmp cycle
not_found:
    xor edi,edi                  //not found --> return   nil
done:
    mov Result,edi
    pop ebx; pop edi;
  end;
end;

function FastColumn2(str: PChar; sep: char; cnt: integer; Pend: PChar; var collen: integer): PChar;
Label cycle,done,zero_len,not_found,get_len;
begin
  asm
    push edi; push ebx;
    mov edi,str                  //edi: string
    mov ecx,Pend; sub ecx,edi    //ecx: length
    jbe not_found   // ? len==0 ?
    or edi,edi; jz not_found // ? nil ?
    mov al,sep                   //al: sep
    mov edx,cnt                  //edx: cnt
    or edx,edx
    jz get_len      // cnt==0 --> return string
cycle:
    repne scasb
    cmp ecx,0
    jbe not_found
    dec edx         // dec cnt
    jnz cycle
get_len:
    mov Result,edi
    cmp al,[edi]
    je zero_len
    inc ecx //xor ecx,ecx; dec ecx //mov ecx,$FFFFFFFF
    mov ebx,ecx;
    repne scasb  // search for column-terminator
    dec ebx; sub ebx,ecx
    jmp done
zero_len:
    xor ebx,ebx
    jmp done
not_found:
    xor ebx,ebx; dec ebx //mov ebx,-1       (chtﱋo by to ebx <-0!, ale Relink v Monitoringu pﱋ tom padﱋ..)
    xor edi,edi                  //not found --> return   nil
    mov Result,edi
done:
    mov eax,collen   // var collen <---> @collen 
    mov [eax],ebx
    pop ebx; pop edi;
  end;
end;

function FastGetColumn(str: PChar; sep: char; cnt: integer; Pend: PChar): string;
var collen: integer;
    pcol: PChar;
Label _NoSuch;
begin
  if (cnt<0) or (str=nil) then goto _NoSuch;
  pcol:=FastColumn2(str,sep,cnt,Pend,collen);
  if (pcol=nil) or (collen=0) then begin _NoSuch: Result:=''; exit; end;
  SetLength(Result,collen); FastCopy(@Result[1],pcol,collen);
end;

function GetColumn(const data: string; sep: char; cnt: integer): string;
var collen: integer; pcol: PChar;
begin
  pcol:=FastColumn2(@data[1],sep,cnt,@data[length(data)+1],collen);
  if (pcol=nil) or (collen=0) then begin Result:=''; exit; end;
  SetLength(Result,collen); FastCopy(@Result[1],pcol,collen);
end;

function LGetColumn(const line: string; sep: char; cnt: integer): string;
var collen: integer; pcol: PChar;
begin
  pcol:=FastColumn2(@line[1],sep,cnt,FastEOL(@line[1]),collen);
  if (pcol=nil) or (collen=0) then begin Result:=''; exit; end;
  SetLength(Result,collen); FastCopy(@Result[1],pcol,collen);
end;

function FastLen(str: PChar): integer; assembler;
asm
  push edi
  mov edi,eax
  xor ecx,ecx; dec ecx //mov ecx,$FFFFFFFF
  or edi,edi; jz @ret_zero
  xor eax,eax
  mov edx,ecx
  repne scasb
  //dec edx //dec eax; dec eax //mov eax,$FFFFFFFE
  mov eax,edx
  sub eax,ecx
  dec eax
  pop edi
  jmp @ret
@ret_zero:
  mov eax,edi
  pop edi
@ret:
end;

// This returns nil if not found #10...
function FastEOL1(str: PChar): PChar;
Label Try_Zero,Not_Zero,Found,Give_Null,Give_Result,Back_Cyc,Ret_Start;
begin
  asm
    push edi; push esi; push ebx;
    mov edi,str
    xor ebx,ebx
    or edi,edi
    jz Give_Null                 // test for nil
    mov esi,edi                  // save start address
Try_Zero:
    mov ecx,100                  // search for #0 in 256 byte increments...
    mov edx,edi                  // Save position
    xor eax,eax
    repne scasb
    or ecx,ecx
    jz Not_Zero
    mov ebx,1                    // Mark #0 was found:
Not_Zero:
    mov eax,100
    sub eax,ecx                 // Search only until #0, if found,   or 256 bytes
    mov edi,edx                 // Search again from saved position
    mov ecx,eax
    nop
    mov eax,$A
    repne scasb
    or ecx,ecx
    jnz Found                   // Not whole 256 walked --> found
    or ebx,ebx
    jz Try_Zero                 // Didn't find #0 before, go for next 256
Give_Null:
    xor edi,edi                 // return(nil)
    jmp Give_Result
Found:
    dec edi                     // point back to #10!
Back_Cyc:
    cmp edi,esi
    jbe Ret_Start               // Don't walk before start!
    dec edi
    mov al,[edi]
    cmp al,$D                   //go back through #13
    je Back_Cyc
    inc edi                     //point to terminating #13
    jmp Give_Result
Ret_Start:
    mov edi,esi
Give_Result:
    mov Result,edi
    pop ebx; pop esi; pop edi;
  end;
end;

// This returns @(#0) if not found #10...
function FastEOL(str: PChar): PChar; assembler;
asm //             EAX->ESI
  test eax,eax
  jnz  @doit
  ret
@doit:
  push esi
  mov  esi,eax
  mov  edx,$00000D0A        // DL=$0A  DH=$0D
@fe_cy:
  lodsb
  or   al,al ; je   @ret_esi_m1
  cmp  al,dl ; je   @ret_esi_m1
  cmp  al,dh ; jne  @fe_cy
@ret_esi_m1:
  dec  esi
@ret_esi:
  mov  eax,esi
  pop esi
end;

{$ifdef UNUSED}
// This returns @(#0) if not found #10...   // erroneous!
function FastEOL_x(str: PChar): PChar;
var pzero_p1: PChar;
Label Try_Zero,Not_Zero,Found,Give_End,Give_Null,Give_Result,Back_Cyc,Ret_Start;
begin
  asm
    push edi; push esi; push ebx;
    mov edi,str
    xor ebx,ebx                  // ebx=1 ---> #0 reached...
    or edi,edi
    jz Give_Null                 // test for nil
    mov esi,edi                  // save start address
Try_Zero:
    mov ecx,100                  // search for #0 in 256 byte increments...
    mov edx,edi                  // Save position
    xor eax,eax
    repne scasb
    or ecx,ecx
    jz Not_Zero
    mov ebx,1                    // Mark #0 was found:
    mov pzero_p1,edi
Not_Zero:
    mov eax,100
    sub eax,ecx                 // Search only until #0, if found,   or 256 bytes
    mov edi,edx                 // Search again from saved position
    mov ecx,eax
    nop
    mov eax,$A
    repne scasb
    or ecx,ecx
    jnz Found                   // Not whole 256 walked --> found
    or ebx,ebx
    jz Try_Zero                 // Didn't find #0 before, go for next 256
Give_End:
    mov edi,pzero_p1
    dec edi
    jmp Give_Result
Give_Null:
    xor edi,edi                 // return(nil)
    jmp Give_Result
Found:
    dec edi                     // point back to #10!
Back_Cyc:
    cmp edi,esi
    jbe Ret_Start               // Don't walk before start!
    dec edi
    mov al,[edi]
    cmp al,$D                   //go back through #13
    je Back_Cyc
    inc edi                     //point to terminating #13
    jmp Give_Result
Ret_Start:
    mov edi,esi
Give_Result:
    mov Result,edi
    pop ebx; pop esi; pop edi;
  end;
end;
{$endif UNUSED}

function FastComp(s1, s2: PChar; len: integer): Boolean; assembler;
asm
  push edi; push esi; push ebx;
  mov esi,eax //esi=s1
  mov edi,edx //edi=s2
  or esi,esi; jz @not_same
  or edi,edi; jz @not_same
  or ecx,ecx; jz @same
  //mov ecx,len //ecx=len
  repe cmpsb // not usefull to implement cmpsd ... usually would compare short strings...
  je @same
@not_same:
  xor eax,eax
  jmp @do_ret
@same:
  xor eax,eax ; inc eax
@do_ret:
  pop ebx; pop esi; pop edi;
end;

function FastCompUntil(s1,s2: PChar; term: char): integer; assembler; // Return logical (s1-s2)
asm
  push edi; push esi; push ebx;
  mov edi,edx; mov esi, eax   // esi=s1, edi=s2
  xor ebx,ebx; xor eax,eax
  or edi,edi; jz @Err_s2  // s2=nil ---> test s1 also...
  or esi,esi; jz @Less_s1 // s1=nil & s2<>nil    ---> s1 is less
  mov bl,cl   // bl=term
@loop001:
  mov al,[esi]
  cmpsb
  jne @002     // different...
  cmp al,bl
  jne @loop001     // loop, if not a terminator...
@Equal:
  xor eax,eax  // equal...
  jmp @fcu_out
@002:            // strings differ... determine, which is "bigger"
  mov ah,[edi-1]
  cmp al,bl
  je @Less_s1   // s1 terminated, not same length --> s1 is Less
  cmp ah,bl
  je @Less_s2   // s2 terminated, not same length --> s2 is Less
  sub al,ah
  //ja @Less_s2 { could be instead of next 2 lines...
  movsx eax,al
  jmp @fcu_out {}
@Less_s1:
  xor eax,eax; dec eax
  jmp @fcu_out
@Err_s2: //<-----  s2=nil ---> test s1 also...
  or esi,esi; jz @Equal
  // fall to Less_s2:
@Less_s2:
  xor eax,eax; inc eax
@fcu_out:
  //mov Result,eax
  pop ebx; pop esi; pop edi;
end;

procedure FastClear(str: Pointer; len: integer); assembler;
asm //              EAX           EDX
  push edi
  mov ecx,edx // ecx=len
  mov edi,eax // edi=str
  xor eax,eax
  shr ecx,2; jz @shorter4
  repne stosd
@shorter4:
  and edx,3; je @CopyDone
  stosb
  dec edx; je @CopyDone
  stosb
  dec edx; je @CopyDone
  stosb (* }
  mov ecx,edx
  or ecx,ecx
  je @CopyDone
  repne stosb // *)
@CopyDone:
  pop edi
end;

procedure FastCopy(dest, src: PChar; len: integer);
begin
  asm
    push edi; push esi; push ebx;
    mov ecx,len
    or ecx,ecx
    jbe @CopyDone
    mov edi,dest
    mov eax,ecx
    mov esi,src
    {$ifdef SAFE} or edi,edi; jz @CopyDone {$endif SAFE}
    shr ecx,2; jz @shorter4
    rep movsd
@shorter4:
    mov ecx,eax
    and ecx,3; jz @CopyDone
    rep movsb
@CopyDone:
    pop ebx; pop esi; pop edi;
  end;
end;

function FastCopyUntil(dest, src: PChar; term: char): integer;
Label cycle;
begin
  asm
    push edi; push esi;
    xor ecx,ecx
    mov ah,term
    mov edi,dest
    mov esi,src
cycle:
    lodsb
    stosb
    inc ecx
    cmp al,ah
    jne cycle
    mov Result,ecx
    pop esi; pop edi;
  end;
end;

function MemDup(src: Pointer; size: integer): Pointer;
begin
  Result:=AllocMem(size);
  FastCopy(Result,src,size);
end;


//in EAX=pbitarr, dx=bit
procedure BitSet(pbitarr: PChar; bit: smallint); assembler;
asm
  bts [eax],dx
end;

procedure BitReset(pbitarr: PChar; bit: smallint); assembler;
asm
  btr [eax],dx
end;

function BitTest(pbitarr: PChar; bit: smallint): Boolean; assembler;
asm
  bt [eax],dx
  jc @is_on
  xor eax,eax
  jmp @99
@is_on:
  xor eax,eax; inc eax
@99:
end;

function Subst(const s,find,replace: AnsiString): AnsiString;
var l,lf,lres,lr,ldif,lmv: integer;
    p0,pst,pfi,pfound,prep,pres,pe: PChar;
begin
  l:=length(s); pfi:=PChar(find); lf:=length(find);
  prep:=PChar(replace); lr:=length(replace); ldif:=lr-lf;
  p0:=PChar(s); pst:=p0; pe:=p0+l;
  //pe^:=#0;
  lres:=l;
  //determine size change:
  while(pst<>nil) do begin
    pst:=StrPos(pst,pfi);
    if pst<>nil then begin inc(lres,ldif); inc(pst,lf); end;
  end;
  //allocate result:
  SetLength(Result,lres);
  if lres=0 then exit;
  pres:=PChar(Result); FastClear(pres,lres);
  //Copy different parts & replace:
  pst:=p0;
  repeat
    pfound:=StrPos(pst,pfi);
    if pfound=nil then begin  //No more occurences found...
      Move(pst^,pres^,pe-pst);                           //Move(Source,Dest,Count);
      if pres<>nil then begin
        inc(pres,pe-pst); pres^:=#0; // Terminate...
      end;
      exit;
    end;
    lmv:=pfound-pst;
    Move(pst^,pres^,lmv); inc(pres,lmv);
    Move(prep^,pres^,lr); inc(pres,lr);
    inc(pst,lmv+lf);
  until False;
end;

function CharSubst(const S: String; Find,Replace: Char): String;
var pc: PChar;
begin
  Result:=S;
  if Find=#0 then exit;
  pc:=PChar(Result);
  if pc=nil then exit;
  while True do begin
    pc:=StrScan(pc,Find);
    if pc<>nil then begin
      pc^:=Replace;
      inc(pc);
      continue;
    end;
    break;
  end;
end;

procedure ParseGeom(s: AnsiString; var l,t,w,h: integer);
//var p: integer;
begin
  if s='' then exit;
  l:=StrToIntDef(ExtractToken(s,','),0);
  if s='' then exit;
  t:=StrToIntDef(ExtractToken(s,','),0);
  if s='' then exit;
  w:=StrToIntDef(ExtractToken(s,','),0);
  if s='' then exit;
  h:=StrToIntDef(ExtractToken(s,','),0);
  {p:=pos(',',s);
  if p>1 then begin l:=StrToIntDef(copy(s,1,p-1),0); s:=Copy(s,p+1,length(s)-p); end;
  p:=pos(',',s);
  if p>1 then begin t:=StrToIntDef(copy(s,1,p-1),0); s:=Copy(s,p+1,length(s)-p); end;
  p:=pos(',',s);
  if p>1 then begin w:=StrToIntDef(copy(s,1,p-1),0); s:=Copy(s,p+1,length(s)-p); end;
  if length(s)>0 then h:=StrToIntDef(s,0);{}
end;

function TitleCase(const s: AnsiString): AnsiString;
var p,l: integer;
    wasSp: Boolean;
begin
  Result:=AnsiLowerCase(s); l:=length(Result); wasSp:=True;
  for p:=1 to l do begin
    if Result[p] in [' ',#9,'_','-'] then wasSp:=True
    else if wasSp then begin
      Result[p]:=AnsiUpperCase(Result[p])[1];
      wasSp:=False;
    end;
  end;
end;

function BreakApart(BaseString: AnsiString; const BreakString: AnsiString; StringList: TStrings): TStrings;
var EndOfCurrentString,LenBreak: integer;
begin
  if BaseString<>'' then
    try
      LenBreak := Length(BreakString);
      while True do begin
        EndOfCurrentString := Pos(BreakString, BaseString);
        if EndOfCurrentString = 0 then begin
          StringList.Add(BaseString);
          break;
        end else
          StringList.Add(Copy(BaseString, 1, EndOfCurrentString - 1));
        //BaseString := Copy(BaseString, EndOfCurrentString + LenBreak, length(BaseString) - EndOfCurrentString);
        Delete(BaseString, 1, EndOfCurrentString + LenBreak - 1);
      end; //until EndOfCurrentString = 0;
    //except on {E:}Exception do {Error('Error occured in BreakApart:'#10+E.Message){}; end;
    except ; end;
 Result := StringList;
end;

function PutTogether1(StrLst: TStrings; const BreakString: AnsiString): AnsiString;
var i,Count: integer;
begin
  try
    Result:='';
    Count:=StrLst.Count;
    if Count=0 then exit;
    Result:=StrLst[0];
    for i:=1 to Count-1 do
      Result:=Result+BreakString+StrLst[i];
  //except on E:Exception do Error('Error occured in PutTogether:'#10+E.Message); end;
  except ; end;
end;

function PutTogether(StrLst: TStrings; const BreakString: AnsiString): AnsiString;
var i,lb,len,Count,Last: integer;
    s: string;
    pc,pb: PChar;
begin
  lb:=Length(BreakString);
  Count:=StrLst.Count;
  // Determine length(Result)
  if Count>1 then len:=(Count-1)*lb else len:=0;
  for i:=0 to Count-1 do
    inc(len,length(StrLst[i]));
  // Alloc result:
  SetLength(Result,len);
  // Copy parts:
  if len>0 then begin
    pc:=@Result[1]; // this calls UniqueString! //pc:=PChar(Result);
    pb:=PChar(BreakString);
    Last:=Count-1;
    for i:=0 to Count-1 do begin
      s:=StrLst[i];
      len:=length(s);
      if len<>0 then begin
        FastCopy(pc,Pointer(s),len);
        inc(pc,len);
      end;
      if (lb<>0) and (i<Last) then begin
        FastCopy(pc,pb,lb);
        inc(pc,lb);
      end;
    end;
    pc^:=#0; // Terminate the string...
  end;
end;

function IcIndexOf(Strings: TStrings; const Item: string): integer; // ignore-case IndexOf ... (AnsiCompareText)
var i: integer;
begin
  for i:=0 to Strings.Count-1 do
    if AnsiCompareText(Item,Strings[i])=0 then begin
      Result:=i;
      exit;
    end;
  Result:=-1;
end;

procedure SListAddFmt(Strings: TStrings; const Fmt: string; const Args: array of const);
var Line: string;
begin
  if Strings=nil then exit;
  FmtStr(Line,Fmt,Args);
  Strings.Add(Line);
end;

function StripComment(const s: AnsiString): AnsiString;
var p: integer;
begin
  p:=Pos('#',s);
  if p>0 then Result:=Copy(s,1,p-1) else Result:=s;
end;

(*  search "s" from position "p" until next s[p], skipping (...) [...] {...} "..." ; \?
    returns that part of "s"
*)
function UntilNext(const s: AnsiString; var p: integer): AnsiString;
var l,st: integer;
    buf: array[0..127] of char; bufpos: integer;
begin
  l:=length(s);
  st:=p;
  buf[0]:=#0; bufpos:=0; {buffer filling starts from 1!}
  repeat
    if s[p]=buf[bufpos] then begin dec(bufpos); inc(p); Continue; end;
    case s[p] of
      '(': begin inc(bufpos); buf[bufpos]:=')'; end;
      '{': begin inc(bufpos); buf[bufpos]:='}'; end;
      '[': begin inc(bufpos); buf[bufpos]:=']'; end;
      '"': begin inc(bufpos); buf[bufpos]:='"'; end;
      '''': begin inc(bufpos); buf[bufpos]:=''''; end;
      '\': inc(p);
    end;
    inc(p);
  until (bufpos<=0) or (p>l);
  if bufpos>0 then raise ETextError.Create('Missing '+buf[1]+' in "'+s+'"');
  if st>=p-2 then Result:='' else Result:=Copy(s,st+1,p-st-2);
end;

function sltSpc(const s: AnsiString): AnsiString; {skip lead&trail spaces}
var ps,pe: integer;
begin
  ps:=1; pe:=length(s);
  while (s[ps] in [' ',#9,#13,#10]) and (ps<pe) do inc(ps);
  while (s[pe] in [' ',#9,#13,#10]) and (pe>ps) do dec(pe);
  if (ps=pe) and (s[ps] in [' ',#9,#13,#10]) then Result:=''
  else Result:=Copy(s,ps,pe-ps+1);
end;

function InsideQuote(const s: AnsiString): AnsiString;
var p:integer;
begin
  p:=Pos('"',s);
  if p<0 then begin Result:=s; exit; end;
  Result:=Copy(s,p+1,length(s)-p);
  p:=Pos('"',Result);
  if p<0 then exit;
  //Result:=Copy(Result,1,p-1);
  SetLength(Result,p-1);
end;

{$ifdef TEXTGRAF}
function ParseColor(s: AnsiString): TColor;
var p: integer;
begin
  Result:=clBlack;
  p:=pos('x',s);
  if p<1 then exit;
  s:='$'+Copy(s,p+1,length(s)-p);
  Result:=StrToIntDef(s,clBlack);
end;

function XColor(clr: TColor): AnsiString;
begin
  Result:=Format('0x%x',[clr]);
end;

{displays multiple texts in format:
text|fontname|fontsize|x|y[|color]
 - color is optional...
}
procedure DisplayTexts(cnv: TCanvas; ls: TStringList);
var i: integer;
    tls: TStringList;
    po: TPoint; p,w,h,fsz: integer; fnm,ftags: AnsiString;
    clr: TColor;
    exFont: TFont;
begin
  if (cnv=nil) or (ls=nil) or (ls.Count=0) then exit;
  exFont:=TFont.Create; tls:=TStringList.Create;
  try
    exFont.Assign(cnv.Font);
    cnv.Brush.Style:=bsClear;
    for i:=0 to ls.Count-1 do begin
      tls.Clear; BreakApart(ls[i],'|',tls);
      try
        try clr:=StrToInt(tls[5]); except on Exception do clr:=clBlack; end;
        po.x:=StrToInt(tls[3]);
        po.y:=StrToInt(tls[4]);
        fsz:=StrToInt(tls[2]);
        fnm:=tls[1];
      except on Exception do Continue; end;
      p:=pos(';',fnm);
      if p>0 then with cnv.Font do begin
        ftags:=Copy(fnm,p+1,length(fnm)-p); fnm:=Copy(fnm,1,p-1);
        if Pos('b',ftags)>0 then Style:=[fsBold] else Style:=[];
        if Pos('u',ftags)>0 then Style:=Style+[fsUnderline];
        if Pos('i',ftags)>0 then Style:=Style+[fsItalic];
      end else cnv.Font.Style:=[];
      cnv.Font.Name:=fnm; cnv.Font.Size:=fsz; cnv.Font.Color:=clr;
      fnm:=tls[0];
      w:=cnv.TextWidth(fnm) div 2;
      h:=cnv.TextHeight(fnm) div 2;
      cnv.TextOut(po.x-w,po.y-h,fnm);
    end;
    cnv.Font.Assign(exFont);
  finally tls.Free; exFont.Free; end;
end;
{$endif TEXTGRAF}

procedure reinils(ls: TStrings);
var i,p: integer;
    s: string;
begin
  for i:=ls.Count-1 downto 0 do begin
    s:=ls[i]; p:=Pos('=',s);
    if p>0 then ls[i]:=Copy(s,p+1,length(s)-p);
  end;
end;

function StrDup(src: PChar): PChar;
var len: integer;
begin
  if src=nil then src:='';
  len:=FastLen(src);
  inc(len); // copy trailing zero too.
  Result:=StrAlloc(len);
  FastCopy(Result,src,len);
end;

function StrDup_L(src: PChar; var len: integer): PChar;
var l: integer;
begin
  if src=nil then src:='';
  l:=FastLen(src); len:=l;
  inc(l); // copy trailing zero too.
  Result:=StrAlloc(l);
  FastCopy(Result,src,l);
end;


procedure CountColRows(data: PChar; sep: char; var ccnt,rcnt: integer; lines: TList);
var c,tot_l,l: integer;
    p,ple: PChar;
    last: Boolean;
begin
  if Data=nil then begin ccnt:=0; rcnt:=0; exit; end;
  tot_l:=FastLen(data);
  p:=Data; ccnt:=1; rcnt:=0; last:=False;
  if lines<>nil then begin lines.Clear; lines.Add(p); end;
  repeat
    {FS}l:=tot_l-(p-data);
    {FS}ple:=FastScan(p,#10,l); //ple:=strscan(p,#10);
    if ple=nil then begin ple:=strscan(p,#0); last:=True; end;
    c:=1;
    repeat
      l:=ple-p;
      {FS} p:=FastScan(p+1,sep,l); //p:=strscan(p+1,sep);
      if p=nil then break;
      if(p<ple) then inc(c);
    until p>=ple;
    inc(rcnt);
    if(c>ccnt) then ccnt:=c;
    p:=ple+1;
    if (lines<>nil) and (not last) then lines.Add(p);
  until last;
end;

procedure CheckAppend(var str: string; ch: Char);
begin
  if (str<>'') and (str[length(str)]<>ch) then str:=str+ch;
end;

procedure AddSlash(var path: string);
begin
  if (path<>'') and (path[length(path)]<>'\') then path:=path+'\';
end;

function StripSlash(const path: string): string;
var l: integer;
begin
  l:=length(path);
  if (l=0) or (path[l]<>'\') then Result:=path
  else Result:=Copy(path,1,l-1);
end;

// Pﱋsko竟 "b匀ﱋ znaky...
procedure SkipBlank(var p: PChar);
begin
  if p=nil then exit;
  while p^ in [' ',#9,#10,#13] do inc(p);
end;

function pSkipBlank(p: PChar): PChar; // is better than SkipBlank <-- no need of stack variable
begin
  if p<>nil then while p^ in [' ',#9,#10,#13] do inc(p);
  Result:=p;
end;

procedure SkipEOL(var p: PChar);
begin
  p:=pSkipEOL(p);
end;

{
function pSkipEOL(p: PChar): PChar;
begin
  if p^=#13 then inc(p);
  if p^=#10 then inc(p);
  Result:=p;
end;{}

function pSkipEOL(p: PChar): PChar; assembler;
asm //            EAX      ->EAX
  test eax,eax
  jz   @done
  xor  edx,edx
  cmp  byte ptr [eax],13
  setz dl
  add  eax,edx
  cmp  byte ptr [eax],10
  setz dl
  add  eax,edx
@done:
end;

function SkipLine(p: PChar): PChar; // = pSkipEOL(FastEOL(p))
begin
  Result:=pSkipEOL(FastEOL(p));
end;

function StrLoadFile(const name: string): string;
var sz: integer;
begin
  with TFileStream.Create(name,fmOpenRead or fmShareDenyNone) do try
    sz:=Size;
    SetLength(Result,sz);
    if sz>0 then ReadBuffer(Result[1],sz);
  finally Free; end;
end;

function StrLoadFileEx(const name: string; existing: Boolean): string;
begin
  try
    if FileExists(name) then
      Result:=StrLoadFile(name)
    else
      if existing then raise Exception.Create('File '+name+' does not exist!')
                  else Result:='';
  except on Exception do if existing then raise else Result:=''; end;
end;

procedure StrStoreFile(const name,data: string);
var bnHidden: Boolean;
    fs: TFileStream;
    attr: integer;
begin
  bnHidden:=False;
 // if (SysUtils.Win32Platform=VER_PLATFORM_WIN32_NT) then           //@002+
  begin
    attr:=FileGetAttr(name);
    if (attr+1<>0){attr<>-1} and ((attr and faHidden)<>0) then begin
      // WinNT denies writes to hidden files?! Why?
      bnHidden:=True;
      FileSetAttr(name,attr and not faHidden);
    end;
  end;
  fs:=nil;
  try
    fs:=TFileStream.Create(name,fmCreate);
    if length(data)>0 then
      fs.WriteBuffer(Pointer(data)^,Length(data));
  finally
    fs.Free;
    // Restore hidden attr on NT:
    if bnHidden then
      FileSetAttr(name,FileGetAttr(name) or faHidden);
  end;
end;

{$ifndef MEDICUS}
function GetResData(const resname: string): string;
begin
  Result:=GetResDataEx(HInstance,resname);
end;

function GetResDataEx(Inst: THandle; const resname: string): string;
var szcf: PChar;
    hresinfo,hmem: THandle;
    size: integer;
begin
  Result:='';
  hmem:=0;
  hresinfo:=FindResource(Inst,PChar(resname),RT_RCDATA);
  if hresinfo=0 then exit;
  try
    hmem:=LoadResource(Inst,hresinfo);
    if hmem<>0 then begin
      size:=SizeOfResource(Inst,hresinfo);
      szcf:=LockResource(hmem);
      if szcf<>nil then
        SetString(Result,szcf,size);
    end;
  finally
    if hmem<>0 then FreeResource(hmem);
  end;
end;
{$endif not MEDICUS}

const EmptyRes:array[false..true,false..true] of integer=((2,-1),(1,0));

{$ifdef UNUSED}
function alComp_Ori(const s1,s2: string; len: integer): Boolean;
begin
  case EmptyRes[length(s1)=0,length(s2)=0] of
    0: Result:=True;
    2:
      //Result:=(AnsiStrLIComp(PChar(s1),PChar(s2),len)=0);
      Result := CompareString(LOCALE_USER_DEFAULT, NORM_IGNORECASE,
        PChar(S1), len, PChar(S2), len) = 2;
    else Result:=False;
  end;
end;
{$endif UNUSED}

function liComp(const s1,s2: string; len: integer): Boolean;
begin
  case EmptyRes[length(s1)=0,length(s2)=0] of
    0: Result:=True;
    2:
      Result:=(StrLIComp(PChar(s1),PChar(s2),len)=0);
    else Result:=False;
  end;
end;

type
  StrRec=record
    alloSz,
    refCnt,
    length: integer;
  end;
const skew=sizeof(StrRec);

// Originally: Delphi.5:system.pas: _LStrPos
// rep* cmpsb~scasb converted to case-insensitive loops

function ICPos(const substr : AnsiString; const s : AnsiString): Integer; assembler;
asm
{     ->EAX     Pointer to substr               }
{       EDX     Pointer to string               }
{     <-EAX     Position of substr in s or 0    }

        TEST    EAX,EAX
        JE      @@noWork

        TEST    EDX,EDX
        JE      @@stringEmpty

        PUSH    EBX
        PUSH    ESI
        PUSH    EDI

        MOV     ESI,EAX                         { Point ESI to substr           }
        MOV     EDI,EDX                         { Point EDI to s                }

        MOV     ECX,[EDI-skew].StrRec.length    { ECX = Length(s)               }

        PUSH    EDI                             { remember s position to calculate index        }

        MOV     EDX,[ESI-skew].StrRec.length    { EDX = Length(substr)          }

        DEC     EDX                             { EDX = Length(substr) - 1              }
        JS      @@fail                          { < 0 ? return 0                        }
        MOV     AL,[ESI]                        { AL = first char of substr             }
        INC     ESI                             { Point ESI to 2'nd char of substr      }

        SUB     ECX,EDX                         { #positions in s to look at    }
                                                { = Length(s) - Length(substr) + 1      }
        JLE     @@fail
@@loop:
        {REPNE   SCASB}
        call    @repne_scasb
        JNE     @@fail
        MOV     EBX,ECX                         { save outer loop counter               }
        PUSH    ESI                             { save outer loop substr pointer        }
        PUSH    EDI                             { save outer loop s pointer             }

        MOV     ECX,EDX
        {REPE    CMPSB}
        call    @repe_cmpsb
        POP     EDI                             { restore outer loop s pointer  }
        POP     ESI                             { restore outer loop substr pointer     }
        JE      @@found
        MOV     ECX,EBX                         { restore outer loop counter    }
        JMP     @@loop

@@fail:
        POP     EDX                             { get rid of saved s pointer    }
        XOR     EAX,EAX
        JMP     @@exit

@@stringEmpty:
        XOR     EAX,EAX
        JMP     @@noWork

@@found:
        POP     EDX                             { restore pointer to first char of s    }
        MOV     EAX,EDI                         { EDI points of char after match        }
        SUB     EAX,EDX                         { the difference is the correct index   }
@@exit:
        POP     EDI
        POP     ESI
        POP     EBX
@@noWork:
        ret

// New sub-functions:

// REPNE SCASB , case insensitive, in  EDI dst string, AL search char, ECX = length(EDI)
//                                 out Z-flag E , if found,  NE if not found
//                                     EDI points after the match
@repne_scasb:
        test ecx,ecx
        jnz   @rs_2
        cmp ecx,1    // return NE
        ret
@rs_2:
        push eax  // save char we search (AL) and work register (AH)...
        cmp al,'a'
        jb  @rs_no_sl_a
        cmp al,'z'
        ja  @rs_no_sl_a
        sub al,$20     // convert to uppercase...
@rs_no_sl_a:           // AL is not a small letter...
        // loop cycle:
@rs_cy:
        mov ah,byte ptr [edi]
        inc edi
        cmp ah,'a'
        jb @rs_no_sl_h
        cmp ah,'z'
        ja @rs_no_sl_h
        sub ah,$20     // convert to uppercase
@rs_no_sl_h:
        dec ecx
        jz @rs_last
        cmp al,ah  // from this comparission the Z flag would be returned, if OK...
        je @rs_done
        jmp @rs_cy

@rs_last:
        cmp al,ah  // from this comparission the Z flag would be returned...
@rs_done:
        pop eax // restore saved register...
        ret

// REPE CMPSB, case insensitive  in  ESI source, EDI dest string, ECX = length
//                               out Z-flag = E if same, NE if not same
//                                   EDI,&ESI points after the match
@repe_cmpsb:
        test ecx,ecx
        jnz  @rc_2
        ret         // length 0: return Equal
@rc_2:
        push eax
@rc_cy:
        lodsb   // mov al,byte ptr [esi] ; inc esi
        cmp al,'a'
        jb @rc_nsl1
        cmp al,'z'
        ja @rc_nsl1
        sub al,$20   // convert to uppercase
@rc_nsl1:
        mov ah,byte ptr [edi]
        inc edi
        cmp ah,'a'
        jb  @rc_nsl2
        cmp ah,'z'
        ja  @rc_nsl2
        sub ah,$20   // convert to uppercase
@rc_nsl2:
        dec ecx
        jz  @rc_last
        cmp al,ah
        je  @rc_cy
        // difference:    return NE
        pop eax
        ret
@rc_last:  // last char... return E ~ NE, don't continue the loop
        cmp al,ah
        pop eax
        ret
end;

// ANSI (single-byte, LOCALE_USER_DEFAULT) functions:
  {
var
  AnsiUpTable:array[#0..#255] of char;
  AnsiLoTable:array[#0..#255] of char;

procedure InitAnsiTables; assembler;
asm
  push edi
  push ebx
  // AnsiUpTable:
  lea edi,AnsiUpTable
  lea ebx,CharUpperA
  call @init_table
  // AnsiLoTable:
  lea edi,AnsiLoTable
  lea ebx,CharLowerA
  call @init_table
  pop ebx
  pop edi
  ret

@init_table:
  // until space:
  xor eax,eax
  xor edx,edx
  mov ecx,$21
@ucy1:
  stosb
  inc eax
  loop @ucy1
  mov dl,al
  // rest of the table:
  mov cl,$DF
@ucy2:
  push edx                     // save source index
  push ecx                     // save count
  push edx                     // push operand to CharUpperA ~ CharLowerA
  call ebx
  pop  ecx                     // restore count
  pop  edx                     // restore source index
  stosb                        // store converted char
  inc  edx                     // next index
  loop @ucy2
  ret
end;

function AnsiUp(a: char): char; assembler;
asm
  lea edx,AnsiUpTable
  xor ecx,ecx
  mov cl,al
  mov al,byte ptr [edx+ecx]
end;

function AnsiLo(a: char): char; assembler;
asm
  lea edx,AnsiLoTable
  xor ecx,ecx
  mov cl,al
  mov al,byte ptr [edx+ecx]
end;

procedure _AnsiCvt;
asm // in: EAX=ptr, EDX=len, EBX=table
  mov  ecx,edx
  test ecx,ecx  // ? zero length ?
  jz @done
  test eax,eax  // ? pc=nil ?
  jnz @doit
@done:
  ret
@doit:
  //push ebx
  push edi
  //lea  ebx,AnsiUpTable
  mov  edi,eax
  xor  eax,eax
@cy:
  mov al,byte ptr [edi]
  xlat
  stosb //mov byte ptr [edi],al ; inc edx
  dec ecx
  jnz @cy
  pop edi
  //pop ebx
end;

procedure AnsiUpStr(pc: PChar; len: integer); assembler;
asm
  push ebx
  lea ebx,AnsiUpTable
  call _AnsiCvt
  pop ebx
end;

procedure AnsiLoStr(pc: PChar; len: integer); assembler;
asm
  push ebx
  lea ebx,AnsiLoTable
  call _AnsiCvt
  pop ebx
end;

function AnsiCmp(p1,p2: PChar; len: integer): integer; assembler;
asm //          EAX EDX        ECX
  push esi
  push edi
  push ebx
  jecxz @same
  test eax,eax
  jz @cmp  // EAX=nil :   if EDX=nil, return 0, if EDX>0, return <0
  test edx,edx
  jz @cmp  // EDX=nil, EAX not nil...   return >0
  mov  esi,eax
  mov  edi,edx
  lea  ebx,AnsiUpTable
  xor  eax,eax
  xor  edx,edx
@cy:
  mov  al,byte ptr [edi] // second string
  inc  edi
  xlat      // this is same, as:    mov AL,byte ptr [EBX+AL]
  mov  dl,al
  lodsb                  // first string :      mov al,byte ptr [esi]; inc esi
  xlat
  cmp al,dl
  jne @cmp
  loop @cy
@same:
  xor eax,eax
  jmp @done
@cmp:
  sub eax,edx
@done:
  pop  ebx
  pop  edi
  pop  esi
end;
  }
(*
function AnsiCompareText(const s1,s2: string): integer;
var l,l1,l2: integer;
begin
  l1:=length(s1);
  l:=l1;
  l2:=length(s2);
  if l2<l then l:=l2;
  Result:=AnsiCmp(Pointer(s1),Pointer(s2),l);
  if (Result=0) {and (l1<>l2)} then
    Result:=l1-l2;
end;   *)
           {
function alComp(const s1,s2: string; len: integer): Boolean;
var l1,l2: integer;
begin
  l1:=length(s1);
  l2:=length(s2);
  if (l1<len) or (l2<len) then begin
    Result:=(l1=l2);
    exit;
  end;
  Result:=AnsiCmp(Pointer(s1),Pointer(s2),len)=0;
end;       }

function AnsiUpperCase(const s1: string): string;
begin
//  Result:=s1;
//  AnsiUpStr(@Result[1],length(Result));  // @Result[1] --> calls UniqueString
  Result := UpperCase(s1);
end;

function AnsiLowerCase(const s1: string): string;
begin
 // Result:=s1;
//  AnsiLoStr(@Result[1],length(Result));  // @Result[1] --> calls UniqueString
  Result:= LowerCase(s1);
end;

// Extension compare:
function extComp(const filename,ext: string): Boolean;
begin
  Result:=(CompareText(ExtractFileExt(filename),ext)=0);
end;

function extCompEx(const filename: string; const exts: Array of string): integer;
var i: integer;
begin
  for i:=low(exts) to high(exts) do begin
    if extComp(filename,exts[i]) then begin Result:=i; exit; end;
  end;
  Result:=-1;
end;

function ExtractToken(var line: string; sep: char): string;
var p: integer;
begin
  asm
    mov edx,line ; mov edx,[edx]
    mov ah,sep
    or edx,edx; jz @end
    xor ecx,ecx
  @et_cy:
    mov al,byte ptr [edx+ecx]
    cmp al,ah ; je @found
    or al,al  ; jz @end
    inc ecx
    jmp @et_cy

  @end:
    xor ecx,ecx ; dec ecx  // -1
  @found:
    mov p,ecx
  end;
  if p<0 then begin
    Result:=line; line:='';
  end else begin
    Result:=Copy(line,1,p);
    Delete(line,1,p+1);
  end;
end;

function pExtractToken(var line: PChar; sep: char): string;
var p: integer;
begin
  asm
    mov edx,line ; mov edx,[edx]
    mov ah,sep
    or edx,edx ; jz @end
    xor ecx,ecx
  @et_cy:
    mov al,byte ptr [edx+ecx]
    cmp al,ah ; je @found
    or al,al  ; jz @end
    inc ecx
    jmp @et_cy

  @end:
    xor ecx,ecx ; dec ecx  // -1
  @found:
    mov p,ecx
  end;
  if p<0 then begin
    Result:=StrPas(line); line:=StrEnd(line);
  end else begin
    SetString(Result,line,p);
    inc(line,p+1);
  end;
end;

function ExtractToken4(var line: string; seps4: PChar): string; // seps4=^array[0..3] of char; (for ex. seps4:=' '#9#10#13; )
var p: integer;
begin
  asm
    push esi
    mov esi,line
    push esi // PUSH line-start
    or esi,esi; jz @end
    mov edx,seps4 ; mov edx,[edx] // load 1st 4 chars only...
    mov ecx,edx
    bswap edx
  @et4_cy:
    lodsb
    cmp al,dh ; je @found
    cmp al,dl ; je @found
    cmp al,ch ; je @found
    cmp al,cl ; je @found
    or al,al  ; jz @end
    jmp @et4_cy

  @end:
    pop eax // clear line-start
    xor esi,esi ; dec esi  // -1
    jmp @out
  @found:
    dec esi     // unget separator...
    pop eax     // POP line-start
    sub esi,eax // get index
  @out:
    mov p,esi
    pop esi
  end;
  if p<0 then begin
    Result:=line; line:='';
  end else begin
    Result:=Copy(line,1,p);
    Delete(line,1,p+1);
  end;
end;

function ExtractQuoted(var p: PChar): string; // moves pointer after closing ", inside ("")->(")
var qcnt,l: integer; //number of inside quotes..., length(Result)
    newp: PChar;
begin
  if p=nil then begin Result:=''; exit; end;
  asm
    mov  edx,&p ; mov edx,[edx]  // EDX = p
    mov  ah,byte ptr [edx] // AH  = '"'   (the quote, p^ )
    xor  ecx,ecx           // ECX = count
    mov  qcnt,ecx
    mov  l,ecx
  @eq_skip:
    inc  edx                        // skip initial quote
  @eq_cy:
    mov  al,byte ptr [edx]
    or   al,al
    jz   @eq_done                   // string ends?
    inc  edx
    cmp  al,ah
    jne  @eq_cy
    inc  qcnt                       // have quote...
    mov  al,byte ptr [edx]
    cmp  al,ah
    je   @eq_skip                   // was double, skip the second too...
    dec  qcnt
  @eq_done:
    mov  ecx,edx
    mov  eax,&p
    sub  ecx,[eax]
    sub  ecx,2
    sub  ecx,qcnt                   // ecx=length_of_inside - qcnt...
    mov  &l,ecx
    mov  &newp,edx
  end;
  if qcnt=0 then begin
    SetString(Result,p+1,l);
    p:=newp;
  end else begin
    SetLength(Result,l);
    asm
      push esi ; push edi
      mov  esi,p ; mov esi,[esi]      // ESI=p
      lodsb                           //      +1        (suck first quote...)
      mov  ah,al                      // AH='"'
      mov  edi,Result ; mov edi,[edi] // EDI=Result
      mov  ecx,&l                     // ecx=length(result)
    @eq_cy2:
      lodsb
      stosb
      cmp  al,ah
      jne  @eq_cy_nquo
      lodsb                           // suck second quote...
    @eq_cy_nquo:
      loop @eq_cy2
      lodsb                           // suck last quote
      mov  &newp,esi
      pop edi  ; pop  esi
    end;
  end;
end;

function ExtractArg(var line: string): string; // "some arg" blabla    ~ arg blabla (Command-line arguments...)
var pc,pln: PChar;
begin
  if line='' then begin Result:=''; exit; end;
  pln:=PChar(line);
  pc:=pSkipBlank(pln);
  if pc^='"' then begin
    Result:=ExtractQuoted(pc);
    pc:=PSkipBlank(pc);
    Delete(line,1,pc-pln);
  end else begin
    if pc>pln then line:=pc; // eat incoming spaces...
    Result:=ExtractToken(line,' ');
  end;
end;

function ExtractLine(var text: PChar): string;
var ple: PChar;
begin
  if (text=nil) or (text^=#0) then begin
    Result:=''; exit;
  end;
  ple:=FastEOL(text);
  SetString(Result,text,ple-text);
  if ple^=#13 then inc(ple);
  if ple^=#10 then inc(ple);
  text:=ple;
end;

function ExtractSectionNames(const inf: string; names: TStrings): integer;
var p,ple: PChar;
    tb: string; po: integer;
begin
  Result:=0;
  if (inf='') then exit;
  p:=@inf[1]; if p=nil then exit;
  repeat
    SkipBlank(p);
    ple:=FastEOL(p);
    if p^='[' then begin
      inc(p); inc(Result);
      if names<>nil then begin
        SetString(tb,p,ple-p);
        po:=Pos(']',tb);
        if po>0 then SetLength(tb,po-1)
        else tb:=Trim(tb);
        names.Add(tb);
      end;
    end;
    SkipEOL(ple);
    p:=ple;
  until (p=nil) or (p^=#0);
end;

function ExtractSections(const inf: string; IniList: TStrings): integer;
var p,ple,pst: PChar;
    sec: string;
begin
  Result:=0;
  if (inf='') then exit;
  p:=@inf[1]; if p=nil then exit;
  pst:=nil;
  repeat
    SkipBlank(p);
    ple:=FastEOL(p);
    if p^='[' then begin
      if pst<>'' then begin
        SetString(sec,pst,p-pst);
        IniList.Add(sec);
      end;
      pst:=p;
      inc(p); inc(Result);
    end;
    SkipEOL(ple);
    p:=ple;
  until (p=nil) or (p^=#0);
  // Add last section:
  if (pst<>nil) and (p<>nil) then begin
    SetString(sec,pst,p-pst);
    IniList.Add(sec);
  end;
end;

function FindInfSection(const inf,section: string): PChar;
var p: PChar;
    l: integer;
begin
  l:=length(section);
  if l=0 then begin Result:=nil; exit; end;
  p:=@inf[1];
  while (p<>nil) and (p^<>#0) do begin
    SkipBlank(p);
    if p^='[' then begin
      Result:=p;
      inc(p);
      if (StrLIComp(p,@section[1],l)=0) and ((p+l)^=']') then exit; // Found...
    end;
    p:=FastEOL(p);
  end;
  Result:=nil;
end;

function ExtractInfSection(const inf,section: string): string;
var p: PChar;
    //l: integer;
    line: string;
begin
  Result:='';
  p:=FindInfSection(inf,section);
  if p<>nil then begin
    p:=FastEOL(p);
    if p^=#13 then inc(p);
    if p^=#10 then inc(p);
  end;
  {l:=length(section);
  if l=0 then exit;
  p:=@inf[1];
  // Find [section]
  while p<>nil do begin
    p:=StrScan(p,'[');
    if p<>nil then begin
      inc(p);
      if (StrLIComp(p,@section[1],l)=0) and ((p+l)^=']') then begin
        p:=FastEOL(p); //inc(p,l);
        if p^=#13 then inc(p);
        if p^=#10 then inc(p);
        break;
      end;
    end;
  end; //if p=nil then exit;
  {}
  // Get values until start of next section:
  while (p<>nil) and (p^<>#0) do begin
    line:=Trim(ExtractLine(p));
    if (line<>'') then case line[1] of
      '[': break;
      ';','#': continue;
      else Result:=Result+line+#13#10;
    end;
  end;
end;

procedure DeleteInfSection(var inf: string; const section: string);
var p,p0,pst: PChar;
begin
  p0:=@inf[1];
  pst:=FindInfSection(inf,section);
  if pst=nil then exit;
  // Get lines until start of next section:
  p:=FastEOL(pst);
  while (p<>nil) and (p^<>#0) do begin
    SkipBlank(p);
    if p^='[' then break;
    p:=FastEOL(p);
  end;
  if (p=nil) or (p^=#0) then SetLength(inf,pst-p0) // delete until end
  else Delete(inf,pst-p0+1,p-pst); // delete section from middle...
end;

procedure ReplaceInfSection(var inf: string; const name,data: string);
var p,p0,pst: PChar;
const secstart=#13#10#13#10'[';
      secend=']'#13#10;
begin
  p0:=@inf[1];
  pst:=FindInfSection(inf,name);
  if pst=nil then begin
    // Add new section:
    inf:=Trim(inf)+secstart+name+secend+data+#13#10;
    exit;
  end;
  // Get lines until start of next section:
  p:=FastEOL(pst);
  while (p<>nil) and (p^<>#0) do begin
    while p^ in [#9,#10,#13,' '] do inc(p); //SkipBlank(p);
    if p^='[' then break;
    p:=FastEOL(p);
  end;
  while (pst>p0) and ((pst-1)^ in [#9,#10,#13,' ']) do dec(pst); // RTrim
  {if (p=nil) or (p^=#0) then begin
    SetLength(inf,pst-p0); // delete until end...
    inf:=inf+secstart+name+secend+data+#13#10;
  end else{} begin
    inf:=Copy(inf,1,pst-p0)+secstart+name+secend+data+#13#10+Copy(inf,p-p0+1,length(inf)-(p-p0)); //StrPas(p);
  end;
end;

procedure MergeInf(var dest: string; const src: string; whole_sections: Boolean);
var secname,sec,val,name: string;
    sls,dvals,svals: TStringList;
    i,max,j,jmax,vi: integer;
    eq: Boolean;
const equal:array[false..true] of string=('','=');
begin
  if not whole_sections then begin
    dvals:=TStringList.Create; svals:=TStringList.Create;
  end else begin
    dvals:=nil; svals:=nil;
  end;
  sls:=TStringList.Create;
  try
    ExtractSectionNames(src,sls);
    max:=sls.Count-1;
    for i:=0 to max do begin
      secname:=sls[i];
      if whole_sections then begin
        sec:=ExtractInfSection(src,secname);
        ReplaceInfSection(dest,secname,sec);
      end else begin
        svals.Text:=ExtractInfSection(src,secname);
        dvals.Text:=Trim(ExtractInfSection(dest,secname));
        jmax:=svals.Count-1;
        for j:=0 to jmax do begin
          val:=svals[j]; if val='' then continue;
          eq:=(Pos('=',val)>0);
          name:=ExtractToken(val,'=');
          vi:=dvals.IndexOfName(name);
          name:=name+equal[eq]+val;
          if vi<0 then dvals.Add(name)
          else dvals[vi]:=name;
        end;
      end;
    end;
  finally
    sls.Free;
    if not whole_sections then begin dvals.Free; svals.Free; end;
  end;
end;

function GetValuePtr(psec,pend: PChar; const name: string): PChar;
var pname: PChar;
    len: integer;
begin
  Result:=nil;
  len:=length(name); if len=0 then exit;
  pname:=PChar(name);
  while (psec<pend) and (psec^<>#0) do begin
    if (AnsiStrLIComp(psec,pname,len)=0) and ((psec+len)^='=') then begin
      Result:=psec+len+1;
      exit;
    end;
    psec:=FastEOL(psec);
    if psec^=#13 then inc(psec);
    if psec^=#10 then inc(psec);
  end;
end;

function GetInfValue(const section,name: string): string;
var psec,pend,pval,pve: PChar;
begin
  Result:='';
  psec:=PChar(section); if (psec=nil) then exit;
  pend:=psec+length(section);
  pval:=GetValuePtr(psec,pend,name);
  if pval<>nil then begin
    pve:=FastEOL(pval);
    SetString(Result,pval,pve-pval);
  end;
end;

procedure SetInfValue(var section: string; const name,value: string); // deletes name, if value empty...
begin
  if length(value)=0 then DeleteInfValue(section,name)
  else SetInfValue2(section,name,value);
end;

procedure SetInfValue2(var section: string; const name,value: string);
var psec,pend,pval,pve: PChar;
    l: integer;
Label _Append;
begin
  l:=length(section);
  if l=0 then goto _Append;
  psec:=PChar(section);
  if (psec<>nil) then begin
    pend:=PChar(section)+l;
    pval:=GetValuePtr(psec,pend,name);
    if pval<>nil then begin
      pve:=FastEOL(pval);
      if length(value)=(pve-pval) then begin
        FastCopy(pval,PChar(value),pve-pval);
      end else begin
        section:=Copy(section,1,pval-psec)+value+Copy(section,pve-psec+1,pend-pve);
      end;
      exit;
    end;
  end;
  // Append new value:
  _Append:
  if l=0 then section:=name+'='+value+#13#10
  else begin
    while (l>0) and (section[l] in [#9,#10,#13,' ']) do dec(l);
    section:=Copy(section,1,l)+#13#10+name+'='+value+#13#10;
  end;
end;

procedure DeleteInfValue(var section: string; const name: string);
var psec,pse,pval,pve: PChar;
begin
  if section='' then exit;
  psec:=PChar(section);
  pse:=psec+length(section);
  // Find value in a section:
  pval:=GetValuePtr(psec,pse,name); if pval=nil then exit; //?Value not present?
  // pve = line-end (or section-end)
  pve:=FastEOL(pval);
  // Seek pval to val-name start:
  dec(pval,length(name)+1);
  // Seek pval to real start of line:
  while (not (pval^ in [#13,#10])) and (pval>psec) do dec(pval);
  if (pval^ in [#13,#10]) then inc(pval);
  // Seek pve after line-end:
  if pve^=#13 then inc(pve);
  if pve^=#10 then inc(pve);
  // Delete selected part:
  Delete(section,pval-psec+1,pve-pval);
end;

function FindFirstInfValue(const section: string; var rec: TInfValueRec): integer;
begin
  rec.sec:=PChar(section);
  if (rec.sec=nil) or (rec.sec^=#0) then begin
    Result:=-1; exit;
  end;
  rec.ptr:=rec.sec; rec.prev:=rec.sec;
  rec.value:=ExtractLine(rec.ptr);
  rec.name:=Trim(ExtractToken(rec.value,'='));
  if (rec.name<>'') and (rec.name[1] in ['#',';']) then begin
    Result:=FindNextInfValue(section,rec);
  end else Result:=0;
end;

function FindNextInfValue(const section: string; var rec: TInfValueRec): integer;
Label _Next;
begin
  if (rec.sec=PChar(section)) and (AnsiStrLIComp(rec.prev,PChar(rec.name),length(rec.name))=0) then begin
    // go further:
    _Next:
    if (rec.ptr=nil) or (rec.ptr^=#0) then begin
      Result:=-1; exit;
    end;
    rec.prev:=rec.ptr;
    rec.value:=ExtractLine(rec.ptr);
    rec.name:=ExtractToken(rec.value,'=');
    if (rec.name<>'') and (rec.name[1] in ['#',';']) then goto _Next;
    Result:=0;
  end else begin
    // re-sync:
    rec.sec:=PChar(section);
    rec.ptr:=GetValuePtr(rec.sec,StrEnd(rec.sec),rec.name);
    if rec.ptr=nil then begin Result:=-1; exit; end; // previous disappeared...
    rec.prev:=rec.ptr;
    ExtractLine(rec.ptr); // skip previous name...
    goto _Next;
  end;
end;

{ TIniStringList }

constructor TIniStringList.Create;
begin
  Duplicates:=dupIgnore;
  Sorted:=True;
  ObjOrder:=True;
  FClean:=True;
end;

procedure TIniStringList.Assign(Source: TPersistent);
begin
  Sorted:=False;
  try
    inherited;
  finally Sorted:=True; end;
end;

{procedure TIniStringList.LoadIni(const IniFile: string);
begin
  SetIni(StrLoadFileEx(IniFile,False));
end; }

{procedure TIniStringList.StoreIni(const IniFile: string);
begin
  StrStoreFile(IniFile,GetIni);
end;  }

function OrderSortCompare(p1,p2: Pointer): integer; assembler; far;
asm //                   EAX EDX        -> EAX
  shr eax,2   // Get rid of sign bit problems...
  shr edx,2
  sub eax,edx //Result:=integer(dword(p1)-dword(p2));
end;
       {
procedure TIniStringList.SetIni(const IniData: string);
var i: integer;
begin
  CloseSection(SecDirty);
  Clear;
  Sorted:=False;
  try
    ExtractSections(IniData,Self);
    if ObjOrder then
      for i:=0 to Count-1 do
        PutObject(i,TObject(i));
  finally Sorted:=True; end;
  FClean:=True;
end;   }

function X_MakeOrd(idx: integer; order: TObject): Pointer; assembler;
asm //           EAX EDX          ->  EAX
  shl edx,16   // Result:=idx or (order shl 16);
  or eax,edx
end;
function X_GetIndex(ptr: Pointer): integer; assembler;
asm //            EAX         -> EAX
  and eax,$0000FFFF
end;

procedure GetSortOrder(isl: TIniStringList; ol: TList);
var i: integer;
begin
  for i:=0 to isl.Count-1 do
    ol.Add(X_MakeOrd(i,isl.GetObject(i)));
  ol.Sort(OrderSortCompare);
end;
      {
function TIniStringList.GetIni: string;
var i: integer;
    ol: TList;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  Result:='';
  if ObjOrder and (Count<64*1024) then begin
    // Order by Objects:
    ol:=TList.Create;
    try
      GetSortOrder(Self,ol);
      for i:=0 to ol.Count-1 do
        Result:=Result+Trim(Get(X_GetIndex(ol[i])))+#13#10#13#10;
    finally ol.Free; end;
  end else
    for i:=0 to Count-1 do
      Result:=Result+Trim(Get(i))+#13#10#13#10;
  if Result<>'' then System.Delete(Result,length(Result)-1,2);
  FClean:=True;
end;    }

{
procedure TIniStringList.MergeIni(const IniData: string; WholeSections: Boolean);
var Temp: TStringList;
begin
  Temp:=TStringList.Create;
  try
    ExtractSections(IniData,Temp);
    MergeIniList(Temp,WholeSections);
  finally Temp.Free; end;
end;    }
 {
procedure TIniStringList.MergeIniList(MergeList: TStringList; WholeSections: Boolean);
var i,res: integer;
    sec,data: string;
    ps,pse: PChar;
    ivr: TInfValueRec;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  try
    Self.Sorted:=False;
    for i:=0 to MergeList.Count-1 do begin
      data:=MergeList[i];
      if data='' then continue; // unprobable...
      // Break section_name & section_data :
      ps:=PChar(data);
      inc(ps); // skip '[':
      pse:=StrScan(ps,']');
      if pse=nil then pse:=FastEOL(ps)+1;
      SetString(sec,ps,pse-ps);
      pse:=FastEOL(ps);
      SkipEOL(pse);
      // Add :
      if WholeSections then
        ReplaceSection(sec,pse)
      else begin
        OpenSection(sec);
        data:=pse;
        res:=FindFirstInfValue(data,ivr);
        while res=0 do begin
          SetValue(sec,ivr.name,ivr.Value);
          res:=FindNextInfValue(data,ivr);
        end;
        CloseSection(SecDirty);
      end;
    end;
  finally Self.Sorted:=True; end;
end;  }

function FmtSecName(const SecName: string): string;
begin
  Result:=Trim(SecName);
  if Result='' then exit;
  if Result[1]<>'[' then Result:='['+Result+']';
end;

function CompareSecName(const sn1,sn2: string): Boolean;
const EmptyComp: array[false..true,false..true] of integer=((1,0),(0,2));
var i: integer;
begin
  i:=EmptyComp[length(sn1)<>0,length(sn2)<>0];
  if i=2 then
    Result:=(AnsiCompareText(FmtSecName(sn1),FmtSecName(sn2))=0)
  else
    Result:=(i<>0);
end;
 {
function TIniStringList.SectionIndex(SecName: string): integer;
begin
  SecName:=FmtSecName(SecName);
  if SecName='' then begin
    Result:=-1;
    exit;
  end;
  Find(SecName,Result);
  if (Result<0) or (Result>=Count) then Result:=-1
  else 
    if not alComp(SecName,Get(Result),length(SecName)) then Result:=-1;
end; }

function TIniStringList.GetSectionName(Index: integer): string;
var pc,pe: PChar;
    sec: string;
begin
  sec:=Get(Index);
  pc:=PChar(sec);
  inc(pc); // skip '['
  pe:=StrScan(pc,']');
  if pe=nil then pe:=FastEOL(pc)+1;
  SetString(Result,pc,pe-pc);
end;

function StripSecHeader(const Sec: string): string;
var pc: PChar;
begin
  pc:=PChar(Sec);
  ExtractLine(pc);
  Result:=pc;
end;
          {
function TIniStringList.GetSectionData(Index: integer): string;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  Result:=StripSecHeader(Get(Index));
end;    }
       {
function TIniStringList.GetSection(const SecName: string): string;
var idx: integer;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  idx:=SectionIndex(SecName);
  if idx<0 then Result:=''
  else begin
    // Strip [Section]#13#10
    Result:=StripSecHeader(Get(idx));
  end;
end;  }

procedure TIniStringList.GetSectionNames(DstList: TStrings; ObjSort: Boolean);
var i: integer;
    ol: TList;
begin
  if ObjSort then begin
    // Order by Objects:
    ol:=TList.Create;
    try
      GetSortOrder(Self,ol);
      for i:=0 to ol.Count-1 do
        DstList.Add(GetSectionName(X_GetIndex(ol[i])));
    finally ol.Free; end;
  end else
    for i:=0 to Count-1 do
      DstList.Add(GetSectionName(i));
end;

{

procedure TIniStringList.ReplaceSection(const SecName,SecData: string);
var idx: integer;
    obj: TObject;
    S: string;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  idx:=SectionIndex(SecName);
  if idx<0 then AddSection(SecName,SecData)
  else begin
    S:=FmtSecName(SecName)+#13#10+SecData;
    if Sorted then begin
      obj:=GetObject(idx);
      Delete(idx);
      AddObject(S,obj);
    end else
      Put(idx,S);
  end;
end;     }
          {
procedure TIniStringList.DeleteSection(const SecName: string);
var idx,order,i,iord: integer;
begin
  if OpenSecName<>'' then CloseSection(SecDirty and not CompareSecName(SecName,OpenSecName));
  idx:=SectionIndex(SecName);
  if idx>=0 then begin
    order:=integer(GetObject(idx));
    Delete(idx);
    if ObjOrder then // Patch orders:
      for i:=0 to Count-1 do begin
        iord:=integer(GetObject(i));
        if iord>=order then begin
          dec(iord);
          PutObject(i,TObject(iord));
        end;
      end;
  end;
end;   }

function TIniStringList.AddSection(const SecName,SecData: string): integer;
var obj: TObject;
begin
  if ObjOrder then
    obj:=TObject(Count)
  else
    obj:=nil;
  Result:=AddObject(FmtSecName(SecName)+#13#10+SecData,obj);
end;
                   {
function TIniStringList.RenameSection(const OldName,NewName: string): Boolean; // True=section order changed!
var idx,nidx: integer;
    data: string;
    obj: TObject;
begin
  Result:=False;
  if OpenSecName<>'' then CloseSection(SecDirty);
  idx:=SectionIndex(OldName);
  if idx>=0 then begin
    data:=StripSecHeader(Get(idx));
    obj:=GetObject(idx);
    DeleteSection(OldName);
    nidx:=AddSection(NewName,data);
    Objects[nidx]:=obj;
    Result:=(nidx<>idx);
  end;
end;
         }
{
procedure TIniStringList.OpenSection(const SecName: string);
begin
  if (OpenSecName<>'') then begin
    if CompareSecName(SecName,OpenSecName) then exit; // already opened...
    CloseSection(SecDirty);
  end;
  OpenSecName:='';
  SecDirty:=False;
  OpenSecData:=GetSection(SecName);
  OpenSecName:=SecName;
end;   }
   {
procedure TIniStringList.CloseSection(Store: Boolean);
var secnm,secdt: string;
begin
  if OpenSecName<>'' then begin
    //if not SecDirty then Store:=False;
    secnm:=OpenSecName;       OpenSecName:='';
    secdt:=Trim(OpenSecData); OpenSecData:='';
    SecDirty:=False;
    if Store then ReplaceSection(secnm,secdt);
  end;
end;      }

{function TIniStringList.GetClean: Boolean;
begin
  if OpenSecName<>'' then CloseSection(SecDirty);
  Result:=FClean;
end; }

procedure TIniStringList.Changed;
begin
  FClean:=False;
  inherited;
end;
      {
function TIniStringList.GetValue(const SecName,ValName: string): string;
begin
  OpenSection(FmtSecName(SecName));
  Result:=GetInfValue(OpenSecData,ValName);
end; }
          {
procedure TIniStringList.SetValue(const SecName,ValName,ValData: string); 
begin
  OpenSection(FmtSecName(SecName));
  SetInfValue(OpenSecData,ValName,ValData);
  SecDirty:=True;
end;
              }
{
procedure TIniStringList.DeleteValue(const SecName,ValName: string); 
begin
  OpenSection(FmtSecName(SecName));
  DeleteInfValue(OpenSecData,ValName);
  SecDirty:=True;
end;       }

//function iMax(a,b: integer): integer; begin if a<b then Result:=b else Result:=a; end;
//function iMin(a,b: integer): integer; begin if a<b then Result:=a else Result:=b; end;
function iMin(a,b: integer): integer; assembler;
asm
  cmp eax,edx
  jle @out
  xchg eax,edx
@out:
end;

function iMax(a,b: integer): integer; assembler;
asm
  cmp eax,edx
  jge @out
  xchg eax,edx
@out:
end;

function ValPC(var pc: PChar; var Digit: integer): LongBool; assembler;
asm //            [EAX]           EDX            ->EAX
  push edi ; push esi ; push ebx
  push eax // remember ptr ^pc (updated <pc> stored at exit...)
  mov  edi,edx                             // EDI = ^digit
  mov  eax,[eax]
  test eax,eax
  jz   @empty
  call pSkipBlank
  mov  esi,eax                             // ESI = string (blanks skipped)
  xor  eax,eax
  xor  ebx,ebx                             // EBX = digit
  xor  ecx,ecx                             // ECX = digit_count
@cy:
  lodsb
  sub  al,'0'
  jb   @done
  cmp  al,9
  ja   @done
  lea  edx,[ebx*2+eax]
  inc  ecx // +digit_count
  lea  ebx,[ebx*8+edx] // ebx=10*ebx+eax
  jmp  @cy

@empty: // nil was passed as <pc> ...
  pop  edx
  xor  eax,eax
  //mov  [edx],eax
  jmp  @out
  
@done:
  mov  [edi],ebx // store digit...
  dec  esi // last char was not parsed...
  pop  edx // pop ^caller`s_pc
  mov  [edx],esi // store changed pc...
  mov  eax,ecx // return LongBool(digit-count) as a Result
@out:
  pop  ebx ; pop  esi ; pop  edi
end;

procedure FmtAddr(res: PChar; p: Pointer);
Label fa_out,fa_cyc;
begin
  asm
    push edi; push ebx
    mov edi,res
    mov ebx,p
    or edi,edi
    jz fa_out
    mov ecx,8
    mov ah,0Fh
    mov dx,$0a69
fa_cyc:
    rol ebx,4
    mov al,ah //0Fh
    and al,bl
    cmp al,dh //0ah
    sbb al,dl //69h
    das
    stosb
    dec cl
    jnz fa_cyc
fa_out:
    pop ebx; pop edi
  end;
end;

procedure OutOfMem(failsize: integer);
begin
  {$ifdef DEBUG}
  if failsize>256 then raise EOutOfMemory.Create('Failed to alloc '+IntToStr(failsize)+' b')
  else
  {$endif}
    OutOfMemoryError; // don't create anything, if < 256 alloc failed!
end;

function AllocArray(n_items: integer): PArgV; assembler;
asm //              EAX             -->EAX
  push eax                // save size for later...
  lea  eax,[eax*4 + 40]    // [eax*4 + 8] exact:, but really alloc +10!
  call SysGetMem
  or   eax,eax
  //jz   OutOfMem //@out_of_mem
  jnz  @allo_ok
  pop  eax
 lea  eax,[eax*4 + 8]
  jz   OutOfMem
@allo_ok:
  pop  ecx
  push edi
  mov  [eax],ecx          // store alloc-count
  lea  edx,[eax+4]
  mov  edi,edx
  xor  eax,eax
  rep  stosd              // fill by   00
  mov  eax,edx
  pop  edi
end;

function ReAllocArray(av: PArgV; n_add: integer): PPChar; assembler;
asm //                EAX        EDX
  or   eax,eax
  jz   @_out
  push edi
  push esi
  //mov  edi,eax        // EDI = av
  mov  esi,edx        // ESI = n_add
  sub  eax,4
  mov  ecx,[eax]      // load size...
  cmp  ecx,edx
  jae  @alo_ok       // ... no need to ReAlloc, have allocated enough free space before
  add  edx,ecx
  push edx
  shl  edx,2          // ... adjust dwords -> bytes
  call SysReAllocMem
  pop  edx
  or   eax,eax
  jz   OutOfMem
  mov  ecx,[eax]    // ecx=old_size
  mov  [eax],edx    // ... store new size
  lea  edi,[eax + ecx*4 + 4] // edi -> 1. new item
  sub  edx,ecx
  mov  ecx,edx      // ecx=new_size - old_size
  mov  esi,eax
  xor  eax,eax
  rep  stosd
  mov  eax,esi
@alo_ok:
  pop  esi
  pop  edi
@_out:
end;

//
// If you think this is too slow, ask Borland, why they didn't export the FreeMem
// procedure and used some obvious "compiler magick" for it...
// In case DelphiMM.DLL is used, U can't use SysFreeMem !
//
{
var mmgr: TMemoryManager;
procedure _FreeMem; // <--- EAX   pointer to free...
asm
  test eax,eax
  je @out
  push eax
  lea eax,mmgr
  call GetMemoryManager
  pop eax
  call mmgr.FreeMem
@out:
end;
     }
{
procedure FreeArray(av: PArgV; itToo: Boolean); assembler;
asm //              EAX        EDX
  or   eax,eax
  jz   @_out                          // --> no array!
  or   edx,edx     // ? items Too ?
  jz   @_free_array
  push edi
  push esi
  push eax
    mov  ecx,[eax-4]
    mov  esi,eax
@fa_cy: // cycle over array-items
      lodsd
      or   eax,eax
      jz   @fa_cy_done
      push ecx
      call StrDispose
      pop  ecx
      loop @fa_cy
@fa_cy_done:
  pop eax
  pop esi
  pop edi
@_free_array:
  call _FreeMem              // free array (only~also)
@_out:
end;
         }
function ArrayAllocCount(av: PArgV): integer; assembler;
asm
  or   eax,eax
  jz   @ret
  mov  eax,[eax-4]
@ret:
end;

function ArrayCount(av: PArgV): integer; assembler;
asm
  or   eax,eax
  jz   @out
  xor  ecx,ecx
  jmp  @ac_in
@ac_cy:
  inc ecx
@ac_in:
  mov  edx,[eax+ecx*4]
  test edx,edx
  jnz  @ac_cy
@ret_ecx:
  mov  eax,ecx
@out:
end;

{$ifdef TXDIALOGS}
const cError=0;
      cWarning=1;
      cInfo=2;
      cConfirm=3;
      cConfirmWarn=4;
      cOnTop=$80000000;

type SimpleButt=(bOk,bOkCancel,bYesNo,bYesNoCancel,bRetryIgnore,bAbortRetryIgnore);
    (*
function Dialog(const msg: string; captp: dword; buts: SimpleButt): integer;
var AWnd: HWnd;
    cap: string;
    flags: integer;
    {$ifndef NOFORMS} WndList: Pointer; {$endif}
const ButFlags:array[SimpleButt] of integer=(
  MB_OK or MB_DEFBUTTON1, // bOk
  MB_OKCANCEL or MB_DEFBUTTON1,//bOkCancel
  MB_YESNO or MB_DEFBUTTON1,//bYesNo
  MB_YESNOCANCEL  or MB_DEFBUTTON1,//bYesNoCancel
  MB_RETRYCANCEL or MB_DEFBUTTON1,//bRetryIgnore
  MB_ABORTRETRYIGNORE or MB_DEFBUTTON2//bAbortRetryIgnore
);
begin
  AWnd:=GetActiveWindow;
  {$ifndef NOFORMS} WndList:=DisableTaskWindows(0); {$endif}
  try
    flags:=MB_APPLMODAL or MB_SETFOREGROUND or ButFlags[buts];
    if (captp and cOnTop)<>0 then begin
      flags:=flags or MB_TOPMOST;
      captp:=captp and not cOnTop;
    end;
    case captp of
      cError: begin cap:=sError; flags:=flags or MB_ICONERROR end;
      cWarning: begin cap:=sWarning; flags:=flags or MB_ICONWARNING end;
      cInfo: begin cap:=sInformation; flags:=flags or MB_ICONINFORMATION end;
      cConfirm: begin cap:=sConfirmation; flags:=flags or MB_ICONQUESTION end;
      cConfirmWarn: begin cap:=sConfirmation; flags:=flags or MB_ICONWARNING; end;
      else begin
        if (captp>1000000) and (not IsBadStringPtr(PChar(captp),4096)) then cap:=PChar(captp)
        else cap:='';
      end;
    end;
    Result:=MessageBoxA(
      {$ifndef NOFORMS}Application.Handle{$else}MDlgOwner{$endif NOFORMS},
      PChar(msg),PChar(cap),flags
    );
  finally
    {$ifndef NOFORMS} EnableTaskWindows(WndList); {$endif}
    SetActiveWindow(AWnd);
  end;
end;
           *)
  (*
procedure Error(const msg: string);
begin
  Dialog(msg,cError,bOk);
end;    *)
     {
procedure Warn(const msg: string);
begin
  Dialog(msg,cWarning,bOk);
end;      }
       {
procedure mdInfo(const msg: string);
begin
  Dialog(msg,cInfo,bOk);
end;
       }
  {
function AnnoyEx(const msg: string; dialogType: TAnnoyType; skipList: TStrings; const item: string; default: Boolean): Boolean;
begin
  Result:=default;
  if (skipList<>nil) and (skipList.IndexOf(item)>=0) then exit;
  case dialogType of
    tmInfo: mdInfo(msg);
    tmWarn: Warn(msg);
    tmYesNo: Result:=YesNo(msg,False);
    tmYesNoWarn: Result:=YesNo(msg,True);
    tmOkCancel: Result:=OkCancel(msg,False);
    tmRetry: Result:=mdRetry(msg);
    tmRetryErr: Result:=mdRetry_Err(msg);
    tmError: Error(msg);
  end;
  if (skipList<>nil) and (GetKeyState(VK_SHIFT)<0) then skipList.Add(item);
end;    }
                {
procedure Annoy(const msg: string; skipList: TStrings; const item: string);
begin
  if (skipList<>nil) and (skipList.IndexOf(item)>=0) then exit;
  mdInfo(msg);
  if (skipList<>nil) and (GetKeyState(VK_SHIFT)<0) then skipList.Add(item);
end;      }

const ConfirmOrWarn:array[false..true] of dword=(cConfirm,cConfirmWarn);
          {
function OkCancel(const msg: string; isWarn: Boolean): Boolean;
var res: integer;
begin
  res:=Dialog(msg,ConfirmOrWarn[isWarn],bOkCancel);
  if res=0 then Abort;
  Result:=(res=IDOK);
end;             }
  {
function YesNo(const msg: string; isWarn: Boolean): Boolean;
var res: integer;
begin
  res:=Dialog(msg,ConfirmOrWarn[isWarn],bYesNo);
  if res=0 then Abort;
  Result:=(res=IDYES);
end;

function YesNoTop(const msg: string; isWarn: Boolean): Boolean;
var res: integer;
begin
  res:=Dialog(msg,ConfirmOrWarn[isWarn] or cOnTop,bYesNo);
  if res=0 then Abort;
  Result:=(res=IDYES);
end;

function YesNoCancel(const msg: string; isWarn: Boolean): integer;
begin
  Result:=Dialog(msg,ConfirmOrWarn[isWarn] or cOnTop,bYesNoCancel);
  if Result=0 then Abort;
end;

function AbortRetryIgnore(const msg: string): integer;
begin
  Result:=Dialog(msg,cConfirmWarn or cOnTop,bAbortRetryIgnore);
  if Result=0 then Abort;
end;

function mdRetry(const msg: string): Boolean;
var res: integer;
begin
  res:=Dialog(msg,cWarning or cOnTop,bRetryIgnore);
  if res=0 then Abort;
  Result:=(res=IDRETRY);
end;

function mdRetry_Err(const msg: string): Boolean;
var res: integer;
begin
  res:=Dialog(msg,cError or cOnTop,bRetryIgnore);
  if res=0 then Abort;
  Result:=(res=IDRETRY);
end;  }
{$endif TXDIALOGS}

function MakeAppName(const name,defPath: string): string;
//var psl: integer;
Label TestName,NoSuch;
begin
  if name='' then begin
    NoSuch:
    Result:=''; exit;
  end;
  if Pos(':',name)>0 then goto TestName;
  if name[1]='\' then goto TestName;
  Result:=ExpandFileName(defPath+name); if FileExists(Result) then exit;
  TestName:
  if FileExists(name) then begin Result:=ExpandFileName(name); exit; end;
  Result:='';
end;

const // Copied from Menus.pas  (Delphi 5: Classes.pas)
  scShift = $2000;
  scCtrl = $4000;
  scAlt = $8000;
       {
procedure SendKeys(const Keys: array of Word);
var OldState,KeybState: TKeyboardState;
    Key: word;
    i,Scan: integer;
  procedure SetKey(Code: Word; Press: Boolean);
  var st: byte;
  begin
    st:=$80;
    if not Press then st:=0;
    KeybState[Code]:=(KeybState[Code] and (not $80)) or st;
  end;
begin
  GetKeyboardState(OldState);
  Move(OldState,KeybState,sizeof(KeybState));
  for i:=Low(Keys) to High(Keys) do begin
    Key:=Keys[i];
    SetKey(VK_SHIFT,(Key and scShift)<>0);
    SetKey(VK_CONTROL,(Key and scCtrl)<>0);
    SetKey(VK_MENU,(Key and scAlt)<>0);
    Key:=(Key and $FF);
    if (Key=0) then continue;
    SetKey(Key,True);
    SetKeyboardState(KeybState);
    Scan:=MapVirtualKey(Key,0);
    keybd_event(Key, Scan, 0, 0);
    SetKey(Key,False);
    keybd_event(Key, Scan, KEYEVENTF_KEYUP, 0);
  end;
  SetKeyboardState(OldState);
end;
           }
//const LedCodes:array[TLedType] of word=(VK_NUMLOCK,VK_SCROLL,VK_CAPITAL);
           {
function SetLedState(led: TLedType; newState: Boolean): Boolean; // returns previous state...
var KeybState: TKeyboardState;
    code: word;
    Scan: integer;
begin
  if led>high(TLedType) then begin
    Result:=False;
    exit;
  end;
  code:=LedCodes[led];
  Result:=(GetKeyState(code)<>0); //GetLedState(led);
  if Result<>newState then begin
    GetKeyboardState(KeybState);
    KeybState[code]:=KeybState[code] xor 1;
    SetKeyboardState(KeybState);
    Scan:=MapVirtualKey(code,0);
    keybd_event(code, Scan, 0, 0);
    keybd_event(code, Scan, KEYEVENTF_KEYUP, 0);
  end;
end;

function GetLedState(led: TLedType): Boolean;
begin
  if led>high(TLedType) then begin
    Result:=False;
    exit;
  end;
  Result:=(GetKeyState(LedCodes[led])<>0);
end;
      }
{$ifdef TEXT_LISTUTIL}

{--------------------------------------------------------------------}
{ List util                                                          }
{--------------------------------------------------------------------}

procedure SaveFree(pobj: pointer);
type PObject=^TObject;
var obj: TObject;
begin
  if pobj=nil then exit;
  obj:=PObject(pobj)^;
  PObject(pobj)^:=nil;
  {$ifdef TOHLE_TO_ASI_TAK_DELA:}
  if obj<>nil then obj.Free;
  {$else -> CHYTREJSI_ZPUSOB}
  asm
    mov eax,&obj
    test eax,eax
    je @none
    mov ecx,[eax]
    cmp ecx,3 ; jz @none // zmena proti TObject.Free: Prevent message:   Access violation.... read of address FFFFFFFF
    MOV     DL,1
    CALL    dword ptr [ECX].vmtDestroy
  @none:
  end;
  {$endif}
end;

procedure SaveFreeMem(pptr: pointer);
type PPointer=^Pointer;
var ptr: Pointer;
begin
  if pptr=nil then exit;
  ptr:=PPointer(pptr)^;
  PPointer(pptr)^:=nil;
  if ptr<>nil then FreeMem(ptr);
end;

procedure ListAdd(var List: TList; Item: Pointer);
begin
  if List=nil then List:=TList.Create;
  List.Add(Item);
end;

function ListAddSingle(var List: TList; Item: Pointer): Boolean;
begin
  if List=nil then List:=TList.Create
  else if List.IndexOf(Item)>=0 then begin
    Result:=False;
    exit;
  end;
  List.Add(Item);
  Result:=True;
end;

function ListAddSingleFirst(var List: TList; Item: Pointer): Boolean;
var idx: integer;
begin
  if List=nil then List:=TList.Create
  else begin
    idx:=List.IndexOf(Item);
    if idx>=0 then begin
      Result:=False;
      if idx>0 then List.Move(idx,0);
      exit;
    end;
  end;
  List.Add(Item);
  Result:=True;
end;

procedure ListRemove(var List: TList; Item: Pointer);
begin
  if List<>nil then begin
    List.Remove(Item);
    if List.Count=0 then SaveFree(@List);
  end;
end;

procedure DestroyList(var List: TList); // with Objects...
var i: integer;
    obj: TObject;
begin
  if List<>nil then begin
    for i:=List.Count-1 downto 0 do try
      obj:=List[i];
      List.Delete(i);
      obj.Free;
    except ; end;
    SaveFree(@List);
  end;
end;

procedure FreeListAndItems(var List: TList; Objects: Boolean);
var i: integer;
    tls: TList;
    ptr: Pointer;
begin
  if List=nil then exit;
  tls:=List;
  List:=nil;
  try
    for i:=tls.Count-1 downto 0 do try
      ptr:=tls[i];
      tls.Delete(i);
      if ptr<>nil then begin
        if Objects then TObject(ptr).Free
                   else FreeMem(ptr);
      end;
    except ; end;
  finally tls.Free; end;
end;

procedure ListFreeByType(var List: TList; Cls: TClass);
var i: integer;
    obj: TObject;
begin
  if List<>nil then begin
    for i:=List.Count-1 downto 0 do try
      obj:=List[i];
      if (obj<>nil) and not (obj is Cls) then continue;
      List.Delete(i);
      obj.Free;
    except ; end;
    if List.Count=0 then SaveFree(@List);
  end;
end;

procedure AssignList(dst,src: TList);
begin
  if dst=nil then exit;
  dst.Clear;
  if src=nil then exit;
  dst.Count:=src.Count;
  Move(src.List^,dst.List^,4*src.Count);
end;
{$endif TEXT_LISTUTIL}

initialization
 // InitAnsiTables;
end.
