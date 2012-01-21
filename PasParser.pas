unit PasParser;
//@001 2011.12.27 Noah Silva = Changes for compiling under FPC
//@002 2011.12.28 Noah Silva = Clean-up,
//                           = Conversion of ugly boolean to pointer typecast.
//                           = Temporarily disabled deallocation of parsers
//@005 2012.01.04 Noah Silva = Fixes.
//@006 2012.01.05 Noah Silva = Debugging.
//@007 2012.01.09 Noah Silva = Debugging.
{$MODE Delphi}

interface
uses
  //Windows,  @001-
  SysUtils, Classes,
  ComUtil,
  textutillite;

const
  cWordChars=['0'..'9','_','A'..'Z','a'..'z'];

function IsWhite(c: Char): Boolean;

{ TParser special tokens }
const
  toEOF     = Chr(0);     //@001=
  toSymbol  = Chr(1);     //@001=
  toString  = Chr(2);     //@001=
  toInteger = Chr(3);     //@001=
  toFloat   = Chr(4);     //@001=
  toWString = Chr(5);     //@001=
  toComment = Chr(8);     //@001=
  toLineComment = Chr(9); //@001=

type
  TPasParser = class(TObject)
  private
    FStream: TStream;
    FOrigin: Longint;
    FBuffer: PChar;
    FBufPtr: PChar;
    FBufEnd: PChar;
    FSourcePtr: PChar;
    FSourceEnd: PChar;
    FTokenPtr: PChar;
    FStringPtr: PChar;
    FSourceLine: Integer;
    FSaveChar: Char;
    FToken: Char;
    FFloatType: Char;
    FWideStr: WideString;
    FCommentPtr: PChar;
    FCommentLen: integer;
    FFileName: string;
    procedure ReadBuffer;
    procedure SkipBlanks;
  public
    constructor Create(Stream: TStream);
    destructor Destroy; override;
    procedure CheckToken(T: Char);
    procedure CheckTokenSymbol(const S: string);
    procedure Error(const Ident: string);
    procedure ErrorFmt(const Ident: string; const Args: array of const);
    procedure ErrorStr(const Message: string);
    procedure HexToBinary(Stream: TStream);
    function NextToken: Char;
    function SourcePos: Longint;
    function TokenComponentIdent: string;
    function TokenFloat: Extended;
    function TokenInt: Int64;
    function TokenString: string;
    function TokenWideString: WideString;
    function TokenComment: string; // only toComment! (not toLineComment)
    function TokenSymbolIs(const S: string): Boolean;
    property FloatType: Char read FFloatType;
    property SourceLine: Integer read FSourceLine;
    property Token: Char read FToken;
    property Stream: TStream read FStream;
    property FileName: string read FFileName write FFileName;
  end;

type
  TPasParseState=(ppsUnit,ppsInterface,ppsUsesIntf,ppsTypes,ppsUsesImpl,ppsImpl,ppsInitFinit,ppsDone,ppsError);
  TUnitType=(utNone,utUnit,utProgram,utPackage,utLibrary);

type
  TUnit=class(TComponent)
  private
    FParseState: TPasParseState;
    FUnitName: string;
    FUnitType: TUnitType;
    FDefs: TStrings;
    FDefStack: TList;
    FUses: TStrings;
    FParserStack: TStringList;
    FFileName: string;
    FDfmExists: Boolean;
    procedure SetDefs(Value: TStrings);
  protected
    FParser: TPasParser;
    FSkipping: Boolean;
    procedure FinishStatement;
    function  NextToken: char; // proper {$ifdef} handling...
    procedure PushDef(IsTrue: Boolean);
    function  PopDef: Boolean;
    procedure UsesUnit(const UsedUnit: string; Intf: Boolean);
    procedure DropParsers;
    procedure IncludeFile(const FileName: string);
    procedure PushParser(const NewFileName: string; NewParser: TPasParser);
    function  PopParser: Boolean;
    function  HandleCommentToken(Token: char): char;
    procedure ParseUses(Intf: Boolean; ProgramMode: Boolean=False);
    procedure AddUnitPath(const UnitName, Location: string); virtual;
    procedure UnitParseStep;
    procedure ProjectParseStep;
    procedure PackageParseStep;
  public
    constructor Create(AOwner: TComponent; const FileName: string); reintroduce; virtual;
    destructor Destroy; override;
    property  Defs: TStrings read FDefs write SetDefs;
    property  Parser: TPasParser read FParser;
    property  ParseState: TPasParseState read FParseState;
    procedure Parse;
    property  FileName: string read FFileName;
    property  UnitType: TUnitType read FUnitType; // valid if (ParseState>ppsUnit)
    property  UnitName: string read FUnitName; // valid if (ParseState>ppsUnit) and (UnitType<>utNone)
    property  UsedUnits: TStrings read FUses;
    property  DfmExists: Boolean read FDfmExists;
  end;

  TProject=class(TUnit)
  private
    function GetUnitPath(const UnitName: string): string;
  protected
    FUnitPaths: TDualStringList;
  public
    property  UnitPath[const UnitName: string]: string read GetUnitPath;
    procedure AddUnitPath(const UnitName,Location: string); override;
  end;

Type
  TBoolObjTrue=Class(TObject)  //@002+
  end;                         //@002+
  TBoolObjFalse=Class(TObject) //@002+
  end;                         //@002+

const
  UnitTypeNames:array[TUnitType] of string=('ERROR','unit','program','package','library');

var
  Parser_ExceptHandler:TNotifyEvent=nil;  // = Application.HandleException...
  BoolObjTrue:TBoolObjTrue;   //@002+
  BoolObjFalse:TBoolObjFalse; //@002+

implementation
uses
  fileutil,    //@001+
  PasParseConst, //@001=
  DbugIntf; //@002+



procedure HandleException(Sender: TObject);
begin
 // if Assigned(Parser_ExceptHandler) then     //@005-
 //   Parser_ExceptHandler(Sender);            //@005-
end;

function IsWhite(c: Char): Boolean;
begin
  Result:=c in [#1..' '];
end;


{ TPasParser }

const
  ParseBufSize = 65536;

  {
procedure BinToHex(Buffer, Text: PChar; BufSize: Integer); assembler;
//@002 should be replaced with standard FPC functions
asm
        PUSH    ESI
        PUSH    EDI
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EDX,0
        JMP     @@1
@@0:    DB      '0123456789ABCDEF'
@@1:    LODSB
        MOV     DL,AL
        AND     DL,0FH
        MOV     AH,@@0.Byte[EDX]
        MOV     DL,AL
        SHR     DL,4
        MOV     AL,@@0.Byte[EDX]
        STOSW
        DEC     ECX
        JNE     @@1
        POP     EDI
        POP     ESI
end;

function HexToBin(Text, Buffer: PChar; BufSize: Integer): Integer; assembler;
asm
        PUSH    ESI
        PUSH    EDI
        PUSH    EBX
        MOV     ESI,EAX
        MOV     EDI,EDX
        MOV     EBX,EDX
        MOV     EDX,0
        JMP     @@1
@@0:    DB       0, 1, 2, 3, 4, 5, 6, 7, 8, 9,-1,-1,-1,-1,-1,-1
        DB      -1,10,11,12,13,14,15,-1,-1,-1,-1,-1,-1,-1,-1,-1
        DB      -1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1,-1
        DB      -1,10,11,12,13,14,15
@@1:    LODSW
        CMP     AL,'0'
        JB      @@2
        CMP     AL,'f'
        JA      @@2
        MOV     DL,AL
        MOV     AL,@@0.Byte[EDX-'0']
        CMP     AL,-1
        JE      @@2
        SHL     AL,4
        CMP     AH,'0'
        JB      @@2
        CMP     AH,'f'
        JA      @@2
        MOV     DL,AH
        MOV     AH,@@0.Byte[EDX-'0']
        CMP     AH,-1
        JE      @@2
        OR      AL,AH
        STOSB
        DEC     ECX
        JNE     @@1
@@2:    MOV     EAX,EDI
        SUB     EAX,EBX
        POP     EBX
        POP     EDI
        POP     ESI
end;                   }

constructor TPasParser.Create(Stream: TStream);
begin
  FStream := Stream;
  GetMem(FBuffer, ParseBufSize);
  FBuffer[0] := #0;
  FBufPtr := FBuffer;
  FBufEnd := FBuffer + ParseBufSize;
  FSourcePtr := FBuffer;
  FSourceEnd := FBuffer;
  FTokenPtr := FBuffer;
  FSourceLine := 1;
  NextToken;
end;

destructor TPasParser.Destroy;
begin
  if FBuffer <> nil then
  begin
    FStream.Seek(Longint(FTokenPtr) - Longint(FBufPtr), 1);
    FreeMem(FBuffer, ParseBufSize);
  end;
end;

procedure TPasParser.CheckToken(T: Char);
begin
  if (Token <> T) then begin
    // Skip comments, if they are not relevant in the moment:
    while (Token = toComment) or (Token = toLineComment) do
      NextToken;
  end;
  if Token <> T then
    case T of
      toSymbol:
        Error(SIdentifierExpected);
      PasParser.toString, toWString:
        Error(SStringExpected);
      toInteger, toFloat:
        Error(SNumberExpected);
    else
      ErrorFmt(SCharExpected, [T]);
    end;
end;

procedure TPasParser.CheckTokenSymbol(const S: string);
begin
  if not TokenSymbolIs(S) then ErrorFmt(SSymbolExpected, [S]);
end;

procedure TPasParser.Error(const Ident: string);
begin
  ErrorStr(Ident);
end;

procedure TPasParser.ErrorFmt(const Ident: string; const Args: array of const);
begin
  ErrorStr(Format(Ident, Args));
end;

resourcestring
  SPasParseError='Error: "%s" on line %d of file %s';

procedure TPasParser.ErrorStr(const Message: string);
begin
  raise EParserError.CreateResFmt(@SPasParseError, [Message, FSourceLine, FFileName]);
end;

procedure TPasParser.HexToBinary(Stream: TStream);
var
  Count: Integer;
  Buffer: array[0..255] of Char;
begin
  SkipBlanks;
  while FSourcePtr^ <> '}' do
  begin
    Count := HexToBin(FSourcePtr, Buffer, SizeOf(Buffer));
    if Count = 0 then Error(SInvalidBinary);
    Stream.Write(Buffer, Count);
    Inc(FSourcePtr, Count * 2);
    SkipBlanks;
  end;
  NextToken;
end;

function TPasParser.NextToken: Char;
var
  I, J: Integer;
  IsWideStr: Boolean;
  P, S: PChar;
Label _Literal;
begin
  SkipBlanks;
  P := FSourcePtr;
  FTokenPtr := P;
  case P^ of
    'A'..'Z', 'a'..'z', '_':
      begin
        Inc(P);
        while P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_'] do Inc(P);
        Result := toSymbol;
      end;
    '#', '''':
      begin
        IsWideStr := False;
        J := 0;
        S := P;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if (i > 255) then IsWideStr := True;
                Inc(J);
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  Inc(J);
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        P := S;
        if IsWideStr then SetLength(FWideStr, J);
        J := 1;
        while True do
          case P^ of
            '#':
              begin
                Inc(P);
                I := 0;
                while P^ in ['0'..'9'] do
                begin
                  I := I * 10 + (Ord(P^) - Ord('0'));
                  Inc(P);
                end;
                if IsWideStr then
                begin
                  FWideStr[J] := WideChar(SmallInt(I));
                  Inc(J);
                end else
                begin
                  S^ := Chr(I);
                  Inc(S);
                end;
              end;
            '''':
              begin
                Inc(P);
                while True do
                begin
                  case P^ of
                    #0, #10, #13:
                      Error(SInvalidString);
                    '''':
                      begin
                        Inc(P);
                        if P^ <> '''' then Break;
                      end;
                  end;
                  if IsWideStr then
                  begin
                    FWideStr[J] := WideChar(P^);
                    Inc(J);
                  end else
                  begin
                    S^ := P^;
                    Inc(S);
                  end;
                  Inc(P);
                end;
              end;
          else
            Break;
          end;
        FStringPtr := S;
        if IsWideStr then
          Result := toWString else
          Result := PasParser.toString;  //@001+
      end;
    '$':
      begin
        Inc(P);
        while P^ in ['0'..'9', 'A'..'F', 'a'..'f'] do Inc(P);
        Result := toInteger;
      end;
    '-', '+', '0'..'9':
      begin
        if (P^ in ['-','+']) and not ((P+1)^ in ['0'..'9']) then begin
          goto _Literal;
        end;
        Inc(P);
        while P^ in ['0'..'9'] do Inc(P);
        Result := toInteger;
        while P^ in ['0'..'9', '.', 'e', 'E', '+', '-'] do
        begin
          Inc(P);
          Result := toFloat;
        end;
        if (P^ in ['c', 'C', 'd', 'D', 's', 'S']) then
        begin
          Result := toFloat;
          FFloatType := P^;
          Inc(P);
        end else
          FFloatType := #0;
      end;
    '/': begin
      if (P+1)^<>'/' then goto _Literal;
      // Comment until EOL:
      Result:=toLineComment;
      while True do begin
        case P^ of
          #10: begin
            Inc(FSourceLine);
            break;
          end;
        end;
        Inc(P);
      end;
    end;
    '{': begin
      // Comment until "}":
      Inc(P); // Skip "}"
      FCommentPtr := P;
      Result:=toComment;
      while True do begin
        case P^ of
          #0: begin
            Error('Unclosed comment!');
            // Could read more data???
          end;
          #10: Inc(FSourceLine);
          '}': begin
            Inc(P);
            break;
          end;
        end;
        Inc(P);
      end;
      FCommentLen:=P-FCommentPtr-1;
    end;
    '(': begin
      if (P+1)^<>'*' then goto _Literal;
      // Comment until "*)"
      Result:=toComment;
      Inc(P,2); // Skip "(*"
      FCommentPtr := P;
      while True do begin
        case P^ of
          #0: begin
            Error('Unclosed comment!');
            // Could read more data???
          end;
          #10: Inc(FSourceLine);
          '*': begin
            Inc(P);
            if P^=')' then begin
              Inc(P);
              break;
            end else
              continue; // must skip Inc(P) at normal loop-end, so that "**)" is recognized!
          end;
        end;
        Inc(P);
      end;
      Inc(P);
      FCommentLen:=P-FCommentPtr-2; 
    end;
    else begin
    _Literal:
      Result := P^;
      if Result <> toEOF then Inc(P);
    end;
  end;
  FSourcePtr := P;
  FToken := Result;
end;

procedure TPasParser.ReadBuffer;
var
  Count: Integer;
begin
  Inc(FOrigin, FSourcePtr - FBuffer);
  FSourceEnd[0] := FSaveChar;
  Count := FBufPtr - FSourcePtr;
  if Count <> 0 then Move(FSourcePtr[0], FBuffer[0], Count);
  FBufPtr := FBuffer + Count;
  Inc(FBufPtr, FStream.Read(FBufPtr[0], FBufEnd - FBufPtr));
  FSourcePtr := FBuffer;
  FSourceEnd := FBufPtr;
  if FSourceEnd = FBufEnd then
  begin
    FSourceEnd := LineStart(FBuffer, FSourceEnd - 1);
    if FSourceEnd = FBuffer then Error(SLineTooLong);
  end;
  FSaveChar := FSourceEnd[0];
  FSourceEnd[0] := #0;
end;

procedure TPasParser.SkipBlanks;
begin
  while True do
  begin
    case FSourcePtr^ of
      #0:
        begin
          ReadBuffer;
          if FSourcePtr^ = #0 then Exit;
          Continue;
        end;
      #10:
        Inc(FSourceLine);
      #33..#255:
        Exit;
    end;
    Inc(FSourcePtr);
  end;
end;

function TPasParser.SourcePos: Longint;
begin
  Result := FOrigin + (FTokenPtr - FBuffer);
end;

function TPasParser.TokenFloat: Extended;
begin
  if FFloatType <> #0 then Dec(FSourcePtr);
  Result := StrToFloat(TokenString);
  if FFloatType <> #0 then Inc(FSourcePtr);
end;

function TPasParser.TokenInt: Int64;
begin
  Result := StrToInt64(TokenString);
end;

function TPasParser.TokenString: string;
var
  L: Integer;
Label _Retry;
begin
_Retry:
  if FToken = toString then
    L := FStringPtr - FTokenPtr
  else if FToken in [toComment,toLineComment] then begin
    NextToken;
    goto _Retry;
  end else
    L := FSourcePtr - FTokenPtr;
  SetString(Result, FTokenPtr, L);
end;

function TPasParser.TokenComment: string;
begin
  if FToken <> toComment then
    Error('No comment!');
  SetString(Result, FCommentPtr, FCommentLen);
end;

function TPasParser.TokenWideString: WideString;
begin
  Result := FWideStr;
end;

function TPasParser.TokenSymbolIs(const S: string): Boolean;
Label _Retry;
begin
_Retry:
  Result := (Token = toSymbol) and SameText(S, TokenString);
  if not Result and (Token in [toComment,toLineComment]) then begin
    NextToken;
    goto _Retry;
  end;
end;

function TPasParser.TokenComponentIdent: string;
var
  P: PChar;
begin
  CheckToken(toSymbol);
  P := FSourcePtr;
  while P^ = '.' do
  begin
    Inc(P);
    if not (P^ in ['A'..'Z', 'a'..'z', '_']) then
      Error(SIdentifierExpected);
    repeat
      Inc(P)
    until not (P^ in ['A'..'Z', 'a'..'z', '0'..'9', '_']);
  end;
  FSourcePtr := P;
  Result := TokenString;
end;

{ TUnit }

constructor TUnit.Create(AOwner: TComponent; const FileName: string);
begin
  inherited Create(AOwner);
  FFileName:=ExpandFileNameUTF8(FileName); { *Converted from ExpandFileName*  }
  FParser:=TPasParser.Create(TFileStream.Create(FFileName,fmOpenRead or fmShareDenyWrite));
  FParser.FileName:=FFileName;
  FDefs:=CreateSortedList(dupIgnore);
  FDefStack := TList.Create; //@002+
  FUses:=TStringList.Create;
  FParserStack := TStringList.Create; //@002
//  FDfmExists:=FileExistsUTF8(ChangeFileExt(FileName); { *Converted from FileExists*  }        //@001-
//FDfmExists:=FileExistsUTF8(ChangeFileExt(FileName, 'DFM')); { *Converted from FileExists*  }  //@001+@002-
  FDfmExists:=FileExistsUTF8(ChangeFileExt(FileName, 'lfm')); { *Converted from FileExists*  }    //@002+
end;

destructor TUnit.Destroy;
begin
  try                  //@002+
 //   DropParsers;     //@002 removing resolves runtime error 210, but leaves memory leak!!
    //SaveFree(@FUses);//@002-
    FUses.Free;        //@002+
    //SaveFree(@FDefs);//@002-
    FDefs.Free;        //@002+
    //SaveFree(@FDefStack); //@002-
    FDefStack.Free;         //@002+
    inherited;
  except               //@002+
    SendDebug('Error in TUnit.Destroy'); //@002+
  end;                 //@002+
end;

procedure TUnit.DropParsers;
var Stm: TStream;
begin
  Try                 //@002+
    //  if FParser<>nil then //@002-
  SendDebug('TUnit.DropParsers:' + ExtractFileName(Self.FileName)+'>>');//@002+
  if Assigned(FParser) then  //@002+
  begin
    If not Assigned(FParser.Stream) then                                        //@002+
      SendDebug('TUnit.DropParsers: Error: FParser.Stream not assigned.')       //@002+
      else                                                                      //@002+
        begin                                                                   //@002+
          Stm:=FParser.Stream;
          Stm.Free;
        end;                                                                    //@002+
//    SaveFree(@FParser); //@002-
   SendDebug('About to free:'+ExtractFileName(FParser.FFileName));  //@002+
   If Assigned(FParser) then FParser.Free;         //@002+
   FParser := nil;                                 //@002+ // just in case
   SendDebug('ok');                                //@002+
  end;
  while PopParser = true do
    DropParsers;
  SendDebug('TUnit.DropParsers:' + ExtractFileName(Self.FileName)+'<<');//@002+
  Except            //@002+
    SendDebug('Error in TUnit.DropParsers');  //@002+
  end;               //@002+
end;

procedure TUnit.FinishStatement;
begin
  if Parser.Token=';' then
    Self.NextToken
  else
  if not Parser.TokenSymbolIs('else') then
  begin
    Parser.CheckTokenSymbol('end');
    Self.NextToken;
    if not Parser.TokenSymbolIs('else') then begin
      Parser.CheckToken(';');
      Self.NextToken;
    end;
  end;
end;

function TUnit.NextToken: char;
begin
  Result:=HandleCommentToken(Parser.NextToken);
end;

Function Bool2Ptr(Const IsTrue:Boolean):Pointer;  //@002+
 begin
 //  RESULT := Pointer(Ord(IsTrue))
   case IsTrue of
     True:Result :=  BoolObjTrue;
     False:Result :=  BoolObjFalse;
   end;
 end;

procedure TUnit.PushDef(IsTrue: Boolean);
var i: integer;
begin
 // ListAdd(FDefStack,Pointer(Ord(IsTrue)));  //@002-
 // IF FDefStack = NIL then FDefStack := TList.Create;
  FDefStack.Add(Bool2Ptr(IsTrue));  //@002+
  if IsTrue then begin
    FSkipping:=False;
    for i:=FDefStack.Count-2 downto 0 do
      if FDefStack[i]=nil then begin
        FSkipping:=True;
        break;
      end;
  end else
    FSkipping:=True;
end;

function TUnit.PopDef: Boolean;
var i,last: integer;
begin
  last:=FDefStack.Count-1;
//  Result:=(FDefStack[last]<>nil);        //@002-
  Result:=(FDefStack[last]=BoolObjTrue);   //@002+
  FDefStack.Delete(last);
  FSkipping:=False;
//  if last=0 then
//    SaveFree(@FDefStack)  //@002-
 // FDefStack.Free          //@002+
//  else
  if last <> 0 then
  begin
    for i:=last-1 downto 0 do
//      if (FDefStack[i]=nil) then        //@002-
      if (FDefStack[i]=BoolObjFalse) then  //@002+
        FSkipping:=True;
  end;
end;

function ExtractWord(var s: string): string;
var p,len: integer;
begin
  len:=Length(s);
  for p:=1 to Len do
    if s[p] in [#1..' '] then begin
      Result:=Copy(s,1,p-1);
      s:=Trim(Copy(s,p+1,MaxInt));
      exit;
    end;
  // only 1 word:
  Result:=Trim(s);
  s:='';
end;

function TUnit.HandleCommentToken(Token: char): char;
var comment: string;
    idx: integer;
Label _Retry;
begin
_Retry:
  case Token of
    toComment: begin
      comment:=Parser.TokenComment;
      if (comment<>'') and (comment[1]='$') then begin
        if liComp(comment,'$ifdef',6) then begin
          ExtractWord(comment);
          comment:=ExtractWord(comment);
          // Check, if this is defined:
          if TStringList(FDefs).Find(comment,idx) then begin
            // defined:
            PushDef(True);
          end else begin
            // not defined:
            PushDef(False);
          end;
        end else
        if liComp(comment,'$ifndef',7) then begin
          ExtractWord(comment);
          comment:=ExtractWord(comment);
          // Check, if this is defined:
          if TStringList(FDefs).Find(comment,idx) then
          begin
            // defined:
            PushDef(False);
          end
          else
          begin
            // not defined:
            PushDef(True);
          end;
        end else
        if liComp(comment,'$define',7) then begin
          if not FSkipping then begin
            ExtractWord(comment);
            comment:=ExtractWord(comment);
            Defs.Add(comment);
          end;
        end else
        if liComp(comment,'$else',5) then begin
          if FDefStack<>nil then begin
            PushDef(not PopDef);
          end else
            Parser.Error('Unknown $else directive');
        end else
        if liComp(comment,'$endif',6) then begin
          if FDefStack<>nil then begin
            PopDef;
          end else
            Parser.Error('Unknown $endif directive');
        end else
        if liComp(comment,'$undef',6) then begin
          if not FSkipping then begin
            ExtractWord(comment);
            comment:=ExtractWord(comment);
            // delete from defs:
            if TStringList(FDefs).Find(comment,idx) then
              FDefs.Delete(idx);
          end;
        end else
        if liComp(comment,'$ifopt',6) then begin
          // Assume False to all opts:  ???
          PushDef(False);
        end else
        if liComp(comment,'$include',8) or liComp(comment,'$i ',3) then begin
          if not FSkipping then begin
            // Included file:
            ExtractWord(comment);
            IncludeFile(ExtractWord(comment));
          end;
        end;
      end;
      // Do next:
      Token:=Parser.NextToken;
      goto _Retry;
    end;
    toLineComment: begin
      Token:=Parser.NextToken;
      goto _Retry;
    end;
    toEOF: begin
      if PopParser then begin
        // End of included file:
        //Token:=Parser.NextToken; // this seems to pop token AFTER {$i file} ???
        goto _Retry;
      end;
      if FSkipping then
        Parser.Error('Unclosed ifdef!');
    end;
    else
      if FSkipping then begin
        Token:=Parser.NextToken;
        goto _Retry;
      end;
  end;
  Result:=Token;
end;

procedure NotImplemented;
begin
  Writeln('Not Implemented');   //@007+
  Abort; //raise Exception.Create('Not implemented');
end;

procedure TUnit.ParseUses(Intf: Boolean; ProgramMode: Boolean=False);
var UnitName: string;
begin
  while Parser.Token<>';' do begin
    if Parser.Token=toSymbol then begin
      UnitName:=Parser.TokenString;
      UsesUnit(UnitName,Intf);
      Self.NextToken; // skip used-unit name...
      if Parser.Token=',' then
        Self.NextToken // skip ","
      else
      if ProgramMode and Parser.TokenSymbolIs('in') then begin
        Self.NextToken; // skip "in"
        Parser.CheckToken(PasParser.toString);
        {if NextToken=toString then} begin // skip unit_location...
          AddUnitPath(UnitName,Parser.TokenString);
          Self.NextToken;
        end;
        if Parser.Token=',' then
          NextToken // skip ","
        else
          Parser.CheckToken(';');
      end else
        Parser.CheckToken(';');
    end;
  end;
end;

procedure TUnit.UnitParseStep;
var ut: TUnitType;
begin
  try
    if (ParseState>ppsUnit) and (UnitType<>utUnit) then
      exit;  // this is a lot gentler                                           //@007+
    //      Abort;                                                              //@007-
    //
    case ParseState of
      ppsUnit: begin
        HandleCommentToken(Parser.Token); // All other tokens would be checked by Self.NextToken...
        // Get "unit name;" or "program name;" or ... :
        FUnitType:=utNone;
        Parser.CheckToken(toSymbol);
        for ut:=Succ(utNone) to High(TUnitType) do
          if Parser.TokenSymbolIs(UnitTypeNames[ut]) then begin
            FUnitType:=ut;
            NextToken;
            // unit's name follows:
            Parser.CheckToken(toSymbol);
            FUnitName:=Parser.TokenString;
            NextToken;
            FinishStatement;
            break;
          end;
        if FUnitType=utNone then
          FParseState:=ppsError
        else
          inc(FParseState); // move to ppsInterface...
      end;
      ppsInterface: begin
        Parser.CheckTokenSymbol('interface');
        NextToken;
        inc(FParseState);
      end;
      ppsUsesIntf: begin
        if Parser.TokenSymbolIs('uses') then begin
          NextToken;
          ParseUses(True);
        end;
        inc(FParseState);
      end;
      ppsTypes: begin
        while True do begin
          if Parser.Token=toEOF then
            raise Exception.Create('Unexpected EOF'); 
          if Parser.TokenSymbolIs('implementation') then begin
            Self.NextToken;
            break;
          end;
          // unit interface section:
          //!!!
          NextToken;
        end;
        inc(FParseState);
      end;
      ppsUsesImpl: begin
        if Parser.TokenSymbolIs('uses') then begin
          NextToken;
          ParseUses(False);
        end;
        inc(FParseState);
      end;
      ppsImpl: begin
//        writeln('ppsImpl');
        inc(FParseState); //@007+
//        NotImplemented; //@007-
      end;
      ppsInitFinit: begin
//        writeln('ppsInitFinit');
        //@007+
        inc(FParseState); //@007+
//        NotImplemented; //@007-
      end;
      ppsError: exit;
    end;
  except on e:exception do
    begin
     FParseState:=ppsError;
     SendDebug('TUnit.UnitParseStep: Error' + e.message); //@005+ //@006=
    end;
//    HandleException(Self);  //@005+
  end;
end;

procedure TUnit.ProjectParseStep;
begin
  try
    case ParseState of
      ppsUnit: Abort; // This should have been done in UnitParseStep...
      ppsInterface:
        FParseState:=ppsUsesIntf;
      ppsUsesIntf: begin
        Parser.CheckTokenSymbol('uses');
        NextToken;
        ParseUses(True,True);
        FParseState:=ppsDone; // Don't try to translate begin..end. block of program...
      end;
      else Abort;
    end;
  except
    FParseState:=ppsError;
    SendDebug('TUnit.ProjectParseStep: Error'); //@005+
//    HandleException(Self); //@005-
  end;
end;

procedure TUnit.PackageParseStep;
begin
  try
    case ParseState of
      ppsUnit: Abort; // This should have been done in UnitParseStep...
      ppsInterface: begin
        if Parser.TokenSymbolIs('requires') then begin
          Parser.NextToken; // skip "requires"
          while Parser.Token<>';' do // just skip anything in requires section...
            Parser.NextToken;
          Parser.NextToken; // skip ";"
        end;
        FParseState:=ppsUsesIntf;
      end;
      ppsUsesIntf: begin
        Parser.CheckTokenSymbol('contains');
        NextToken;
        ParseUses(True,True);
        // Done:
        FParseState:=ppsError;
        if Parser.TokenSymbolIs('end') then begin
          Parser.NextToken;
          if Parser.Token='.' then
            FParseState:=ppsDone;
        end;
      end;
      else Abort;
    end;
  except
    FParseState:=ppsError;
    SendDebug('TUnit.PackageParseStep: Error.'); //@005+
 //   HandleException(Self);  //@005-
  end;
end;

procedure TUnit.SetDefs(Value: TStrings);
begin
  FDefs.Clear;
  FDefs.AddStrings(Value);
end;

procedure TUnit.Parse;
begin
  try
  try
//    ChDir(ExtractFilePath(FFileName));                                        //@002-
    SetCurrentDirUTF8(ExtractFilePath(FFileName));                              //@002+
    if ParseState=ppsUnit then
      UnitParseStep;
    case UnitType of
      utNone: exit;
      utUnit:
        while (ParseState < ppsDone) do
          UnitParseStep;
      utProgram,utLibrary: begin
        while (ParseState < ppsDone) do
          ProjectParseStep;
      end;
      utPackage: begin
        while (ParseState < ppsDone) do
          PackageParseStep;
      end;
    end;
  finally
   // DropParsers;  //@002 runtime error 210
  end;

  except //@002+
    SendDebug('TUnit.Parse: Error.');
  end;   //@002+
end;

procedure TUnit.UsesUnit(const UsedUnit: string; Intf: Boolean);
begin
  FUses.AddObject(
    LowerCase(UsedUnit),  // Force to Lowercase                                   //@002=
      Bool2Ptr(Intf));    //@002+
    //    TObject(ord(Intf)));  //@002 - Seriously?  There are better ways to store a boolean
end;

procedure TUnit.IncludeFile(const FileName: string);
var FullName: string;
begin
  FullName:=ExpandFileNameUTF8(ExtractFilePath(Self.FFileName)+FileName); { *Converted from ExpandFileName*  }
  if FileExistsUTF8(FullName) { *Converted from FileExists*  } then
    PushParser(FullName,
      TPasParser.Create(TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite)));
end;

procedure TUnit.PushParser(const NewFileName: string; NewParser: TPasParser);
begin
  if FParserStack=nil then FParserStack:=TStringList.Create;
  FParserStack.AddObject(Self.FileName,FParser);
  Self.FFileName:=NewFileName;
  FParser:=NewParser;
  HandleCommentToken(FParser.Token);
end;

function TUnit.PopParser: Boolean;
var
    Count:Integer;  //@002+
    idx: integer;
begin
  try                                      //@002+
  //  Result:=FParserStack<>nil;           //@002-
  If Not Assigned(FParserStack) then
    begin
      SendDebug('TUnit.PopParser: FParserStack not allocated'); //@002+
      Result := False; //@002+
      exit; //@002+
    end;
//  Result:=FParserStack.Count>0;          //@002-
  Count := FParserStack.Count;             //@002+
  Result := Count > 0;                     //@002+
  if Result then
    begin
      idx := Count - 1;                    //@002=
//    FParser:=TPasParser(FParserStack.Objects[idx]);    //@002-
      If not Assigned(FParserStack.Objects[idx]) then                           //@002+
        SendDebug('TUnit.PopParser: Error: FParserStack.Objects['
                   + IntToStr(idx)+'] not assigned.')  //@002+
      Else
       FParser:=FParserStack.Objects[idx] as TPasParser;  //@002+
      FFileName:=FParserStack[idx];
      SendDebug('TUnit.PopParser: '+ExtractFileName(Self.FileName)+ ': Popped '+FFileName);
      FParserStack.Delete(idx);
  //  if idx=0 then                  //@002-
//      SaveFree(@FParserStack);   //@002-
  //    FParserStack.Free;           //@002+ //@002-
    end
  else                                              //@002+
   SendDebug('TUnit.PopParser: ' + ExtractFileName(Self.Filename)+ ': Nothing to Pop.');   //@002+

  Except
    SendDebug('Error in TUnit.PopParser'); //@002+
  end;                                     //@002+
end;

procedure TUnit.AddUnitPath(const UnitName, Location: string);
begin
  //nic...
end;

{ TProject }

procedure TProject.AddUnitPath(const UnitName, Location: string);
begin
  if FUnitPaths=nil then begin
    FUnitPaths:=TDualStringList.Create;
    FUnitPaths.Sorted:=True;
    FUnitPaths.Duplicates:=dupIgnore;
  end;
  FUnitPaths.AddValue(UnitName,ExpandFileNameUTF8(Location) { *Converted from ExpandFileName*  });
end;

function TProject.GetUnitPath(const UnitName: string): string;
begin
  if FUnitPaths=nil then
    Result:=''
  else
    Result:=FUnitPaths.ValuesByName[UnitName];
end;

initialization //@002+
  BoolObjFalse := TBoolObjFalse.Create;      //@002+
  BoolObjTrue := TBoolObjTrue.Create;        //@002+

finalization
  BoolObjFalse.Free;    //@002+
  BoolObjTrue.Free;     //@002+

end.
