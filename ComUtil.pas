//@001 2011.12.27 Noah Silva = Changes for FPC
//@002 2011.12.29 Noah Silva = Changes for FPC/Mac OS X, Debugging
//@003 2011.12.31 Noah Silva = Clean-up for compiling on Win32
    {$mode Delphi}
unit ComUtil;
interface
uses
  {$IFDEF WIN32}
  Windows,   //@002-
  Messages,  //@002-
  {$ENDIF}
  SysUtils,
  Classes;

procedure ListAdd(var List: TList; Item: Pointer);
function  ListAddSingle(var List: TList; Item: Pointer): Boolean;
function  ListAddSingleFirst(var List: TList; Item: Pointer): Boolean;
procedure FreeListAndItems(var List: TList; Objects: Boolean); // with Objects~Pointers
procedure AssignList(dst,src: TList);

function  GetTempFile(const prefix: string): string;
procedure FinalDeleteFile(const FileName: string);
procedure FinalDeleteFiles(List: TStrings); // Files in List must be expanded to full path!
procedure RemoveFinalDelete(const FileName: string);
procedure DeleteFiles(List: TStrings; bnCanFinal: Boolean{$ifdef VER130}=True{$endif});

function  CreateSortedList(Duplicates: TDuplicates): TStringList;

function  GetSerial(var LastValue: integer): integer;

// SaveFree a IsClass testujû—(kromû—jinÈho):
// - ûe n·zev tû—dy objektu zaËÌnû—na "T" nebo "t"
// - ûe n·zev rodiËovskû—tû—dy rovnû— zaËÌnû—na "T" nebo "t"
// - ûe oba n·zvy jsou kratöÌ neû—40 znakû—
// (Takûe napû— TMultiReadExclusiveWriteSynchronizer je jeötû—OK)

{$IFDEF WIN32}//@002+
procedure ListFreeByType(var List: TList; Cls: TClass);
procedure DestroyList(var List: TList); // with Objects...
function  ListRemove(var List: TList; Item: Pointer): integer;
procedure FreeStrListAndItems(var Strings; Objects: Boolean); // with Objects~Pointers
function  Is_Object(Obj: TObject): Boolean;
procedure SaveFree(pobj: pointer);     // Usage:    SaveFree(@Object)
function  IsClass(Cls: TClass): LongBool; register;
{$ENDIF}
function  ClassIs(Have,Want: TClass): Boolean;
procedure SaveFreeMem(pptr: pointer);  // Usage: SaveFreeMem(@Pointer)

{$ifndef VER130}
procedure FreeAndNil(var Obj);
{$endif}

// StackAlloc and StackFree borrowed from Borland's Grids.pas ...
function StackAlloc(Size: Integer): Pointer; register;
function StackAllocClear(Size: Integer): Pointer; register;
procedure StackFree(P: Pointer); register;

function ExtractToken(var line: string; sep: char): string;
function PeekToken(const Line: string; sep: char): string;
function pExtractToken(var line: PChar; sep: char): string;
function FastEOL(str: PChar): PChar; register;
function ExtractLine(var text: PChar): string;
function IcIndexOf(List: TStrings; const Item: string): integer;

const Lo_Color: Boolean=False;
      BitsPerPixel:byte=0;

{$IFDEF WIN32}//@002+
procedure InitColor;
function Is256Color: Boolean;
{$ENDIF}//@002+

type TBadUnallocProc=procedure(BadObject: TObject; CallAddr: Pointer);

const BadUnalloc:TBadUnallocProc=nil;

type
  EWarning=class(Exception); // Error of user, not of the program...

{----------------------------------------------------------}
{ Special lists:                                           }
{----------------------------------------------------------}

type

  { TDualStringList }

  P2StringItem = ^T2StringItem;
  T2StringItem=record
    Name,Value: string;
  end;
  P2StringItemList = ^T2StringItemList;
  T2StringItemList = array[0..MaxListSize] of T2StringItem;

  TDualStringList=class;
  
  TDualStringListSortCompare=function(List: TDualStringList; Index1, Index2: integer): integer;

  TDualStringList=class(TStrings)
  private
    FList: P2StringItemList;
    FCount: Integer;
    FCapacity: Integer;
    FSorted: Boolean;
    FDuplicates: TDuplicates;
    FOnChange: TNotifyEvent;
    FOnChanging: TNotifyEvent;
    procedure ExchangeItems(Index1, Index2: Integer);
    procedure Grow;
    procedure QuickSort(L, R: Integer; SCompare: TDualStringListSortCompare);
    procedure SetSorted(Value: Boolean);
    // New:
    function  GetUpdateCount: integer;
    function  GetValueN(const Name: string): string;
    procedure SetValueN(const Name, Value: string);
    function  GetIniText: string;
    procedure SetIniText(const Value: string);
  protected
    procedure Changed; virtual;
    procedure Changing; virtual;
    function  Get(Index: Integer): string; override;
    function  GetCapacity: Integer; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override; // returns NIL
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override; // do nothing...
    procedure SetCapacity(NewCapacity: Integer); override;
    procedure SetUpdateState(Updating: Boolean); override;
    function  GetValue(Index: integer): string;
    procedure PutValue(Index: integer; const Value: string);
    // New:
    procedure InsertItem(Index: Integer; const S: string);
    procedure ErrorRes(Msg: PResStringRec; Data: Integer);
  public
    destructor Destroy; override;
    function  Add(const S: string): Integer; override;
    procedure Clear; override;
    procedure Delete(Index: Integer); override;
    procedure Exchange(Index1, Index2: Integer); override;
    function  Find(const S: string; var Index: Integer): Boolean; virtual;
    function  IndexOf(const S: string): Integer; override;
    function  IcIndexOf(const S: string): Integer; // case-insensitive IndexOf...
    function  IndexOfValue(const S: string): Integer;
    procedure Insert(Index: Integer; const S: string); override;
    procedure Sort; virtual;
    procedure Assign(Source: TPersistent); override;
    procedure CustomSort(Compare: TDualStringListSortCompare); virtual;
    property  Duplicates: TDuplicates read FDuplicates write FDuplicates;
    property  Sorted: Boolean read FSorted write SetSorted;
    property  OnChange: TNotifyEvent read FOnChange write FOnChange;
    property  OnChanging: TNotifyEvent read FOnChanging write FOnChanging;
    procedure SwapNamesAndValues;
    // New:
    property  List: P2StringItemList read FList;
    property  Values[Index: integer]: string read GetValue write PutValue;
    property  ValuesByName[const Name: string]: string read GetValueN write SetValueN;
    property  UpdateCount: integer read GetUpdateCount;
    function  AddValue(const Name,Value: string): Integer; virtual;
    procedure InsertValue(Index: integer; const Name,Value: string); virtual;
    property  IniText: string read GetIniText write SetIniText;
  end;

  T2SList=TDualStringList;

  // SubStrings   - a sub-set of another strings...
  // - is thread-safe with other SubStrings over same master
  // - you cannot operate on a Master, while SubStrings exists
  // - if you need to operate on a Master, create another sub-strings or call appropriate Notifications...
  // - high-bound is exclusive (ie. ToIndex is NOT part of subset)...
  // - ToIndex<0 in Create : until Master.Count

  TssNotification=(ssnInsert,ssnMove,ssnDelete);

  TSubStrings=class(TStrings)
  private
    FMaster: TStrings;
    FFirst,FLast: integer;
    FReadOnly: Boolean;
    FColleagues: TList;
    procedure DoDelete(Index: integer);
    procedure SetFirst(Value: integer);
    procedure SetLast(Value: integer);
  protected
    procedure Notify(Reason: TssNotification; Index1,Index2: integer); // in native indexes...
    procedure IndexError(Index: integer);
    procedure ReadOnlyError;
    function  Get(Index: Integer): string; override;
    function  GetCount: Integer; override;
    function  GetObject(Index: Integer): TObject; override;
    procedure Put(Index: Integer; const S: string); override;
    procedure PutObject(Index: Integer; AObject: TObject); override;
  public
 //   constructor Create(Master: TStrings; FromIndex,ToIndex: integer); // high-bound is exclusive!
    {$IFDEF WIN32}//@002+
    destructor Destroy; override;
    {$ENDIF}//@002+
 //   procedure Lock;
 //   procedure UnLock;
    procedure Notification(Reason: TssNotification; Index1,Index2: integer); virtual; // in master indexes...
    //
 //   procedure Assign(Source: TPersistent); override;
 //   procedure Clear; override; {L}
 //   procedure Delete(Index: Integer); override; {L}
 //   procedure Insert(Index: Integer; const S: string); override; {L}
 //   procedure Move(CurIndex, NewIndex: Integer); override; {L}
    procedure Exchange(Index1, Index2: Integer); override;
    // Native props:
    property  FirstLine: integer read FFirst write SetFirst; // relative to Master...
    property  LastLine: integer read FLast write SetLast; // rel.to M... Exclusive! Is not part of these SubStrings!
    property  Master: TStrings read FMaster;
    property  ReadOnly: Boolean read FReadOnly write FReadOnly;
    property  Colleagues: TList read FColleagues;
  end;
  {L} // these functions Lock other threads and notify other sub-strings...
      // Also other functions lock threads - for ex. Add is routed via Insert...

  // TSubFiller - TSubStrings used to Clear&Append into part of large strings, without actually moving 100000 other lines, if not really needed...
  //            - until their window is filled, they overwrite master items, then insert...
  //            - Cut deletes master items, which were not replaced... (is called from Destroy!)
  TSubFiller=class(TSubStrings)
  protected
    FCeiling: integer;
    FInitObjects: Boolean;
  public
 //   constructor Create(Master: TStrings; FromIndex,ToIndex: integer); // high-bound is exclusive! //@001+
 //   destructor Destroy; override;
//    procedure Cut; {L} // delete from master lines between last..ceiling...
    procedure Notification(Reason: TssNotification; Index1,Index2: integer); override; // in master indexes...
//    function  Add(const S: string): Integer; override; {L}
    procedure Clear; override;
 //   procedure Insert(Index: Integer; const S: string); override; {L} // EXC! Cannot insert in middle...
//    procedure Delete(Index: Integer); override; {L}
    property  InitObjects: Boolean read FInitObjects write FInitObjects default False; // wheather Add should set objects to NIL... Is faster, if =False, but there may be some objects from previous item at that position...
    property  Ceiling: integer read FCeiling write FCeiling; 
  end;

//function  SubStringsCount(Master: TStrings): integer;
//procedure DestroySubStrings(Master: TStrings);
function  GetSubsMaster(Source: TStrings; var Offset: integer): TStrings; // while (Source is TSubStrings) do Source:=Source.Master;

type
  // TStringList, used to hold Named, Ref-Counted interfaces...
  TInterfaceItem=record
    FString: string;
    FIntf: IUnknown;
  end;
  PInterfaceItem=^TInterfaceItem;

  TInterfaceItemArray=array[0..MaxListSize] of TInterfaceItem;
  PInterfaceItemArray=^TInterfaceItemArray;

  TNamedInterfaceList=class(TStringList)  // Is not exactly thread-safe... You can use Classes.TInterfaceList instead...
  private
 //   function  GetIntf(Index: integer): IUnknown;
//    procedure PutIntf(Index: integer; const Value: IUnknown);
  protected
    procedure PutObject(Index: Integer; AObject: TObject); override; // does nothing...
    function  GetObject(Index: Integer): TObject; override; // returns nil...
  public
  //  destructor Destroy; override;
 //   procedure Clear; override;
 //   procedure Delete(Index: Integer); override;
 //   procedure Move(CurIndex, NewIndex: Integer); override;
    // New:
 //   function  AddIntf(const Name: string; const Intf: IUnknown): integer; // Use instead of AddObject !
 //   procedure InsertIntf(Index: integer; const Name: string; const Intf: IUnknown);
  //  property  Intf[Index: integer]: IUnknown read GetIntf write PutIntf;
  end;

  TDualListItem=record
    Item,Value: Pointer;
  end;
  PDualListItem=^TDualListItem;

  TDualListItemArray=array[0..MaxListSize] of TDualListItem;
  PDualListItemArray=^TDualListItemArray;

  //---------------------------------------------------------
  // TDualList
  // TList with 2 pointers per item...  Called "Item" & "Value"
  
  TDualList=class(TObject)
  private
    FList: PDualListItemArray;
    FCount: Integer;
    FCapacity: Integer;
    function  GetId(Index: integer): integer;
    function  GetIValue(Index: Integer): integer;
    procedure PutId(Index: integer; Value: integer);
    procedure PutIValue(Index: Integer; Value: integer);
  protected
    function  Get(Index: Integer): Pointer;
    function  GetValue(Index: integer): Pointer;
    procedure Grow; virtual;
    procedure Put(Index: Integer; Item: Pointer);
    procedure PutValue(Index: Integer; Value: Pointer);
    procedure SetCapacity(NewCapacity: Integer);
    procedure SetCount(NewCount: Integer);
  public
    destructor Destroy; override;
    function  Add(Item: Pointer): Integer;
    function  AddValue(Item,Value: Pointer): Integer;
    // Variations, which just spare Pointer(int_value) type-overrides:
    function  AddPiValue(Item: Pointer; Value: integer): integer;
    function  AddIiValue(Item,Value: integer): integer;
    function  AddIpValue(Item: integer; Value: Pointer): integer;
    procedure Clear; virtual;
    procedure Delete(Index: Integer);
    class procedure Error(const Msg: string; Data: Integer); virtual;
    class procedure ErrorRes(Msg: PResStringRec; Data: Integer);
    procedure Exchange(Index1, Index2: Integer);
    function  Expand: TDualList;
    function  First: Pointer;
    function  IndexOf(Item: Pointer): Integer;
    function  IndexOfValue(Value: Pointer): Integer;
    procedure Insert(Index: Integer; Item: Pointer);
    procedure InsertValue(Index: Integer; Item,Value: Pointer);
    function  Last: Pointer;
    procedure Move(CurIndex, NewIndex: Integer);
    function  Remove(Item: Pointer): Integer;
    function  RemoveValue(Value: Pointer): Integer;
    procedure Pack;
    procedure Sort(Compare: TListSortCompare);
    property  Capacity: Integer read FCapacity write SetCapacity;
    property  Count: Integer read FCount write SetCount;
    property  Items[Index: Integer]: Pointer read Get write Put; default;
    property  Values[Index: Integer]: Pointer read GetValue write PutValue;
    property  ID[Index: integer]: integer read GetId write PutId;
    property  IV[Index: Integer]: integer read GetIValue write PutIValue;
    property  List: PDualListItemArray read FList;
  end;

  //---------------------------------------------------------
  // TSortedList - TList, sorted û—la TStringList...

  TSortedList=class(TList)
  private
    FCompare: TListSortCompare;
    FDuplicates: Boolean;
    procedure SetCompare(const Value: TListSortCompare);
  protected
    function  AddItem(Item: Pointer): Integer;
    procedure InsertItem(Index: integer; Item: Pointer);
    procedure MoveItem(CurIndex, NewIndex: Integer);
  public
    function  Add(Item: Pointer): Integer; reintroduce; virtual;
    procedure Assign(OtherList: TList);
    function  Find(Item: Pointer; var Index: integer): Boolean; virtual;
    // These just raise an exception:
    procedure Insert(Index: Integer; Item: Pointer); reintroduce;
    procedure Move(CurIndex, NewIndex: Integer);
    // Properties:
    property  Compare: TListSortCompare read FCompare write SetCompare;
    property  Duplicates: Boolean read FDuplicates write FDuplicates;
  end;

procedure SListAdd(var List: TSortedList; Item: Pointer);
//function SListRemove(var List: TSortedList; Item: Pointer): integer;


type
  //---------------------------------------------------------
  // SubStream - window into another stream...
  //           - is NOT thread-safe... use other locking mechanism instead
  //           - Master must be seekable
  //           - use instead of copying part of a stream into memory-stream...
  //           - Offset<0 in constructor: uses current position as an offset...
  //           - Size<0 in constructor: From offset until end of Master...
  //           - if not ReadOnly, only into the window can be written,
  //           - if WarnOverflow, ESubStreamOverflow is raised, if trying to write more data,
  //             than fits into the window, else the overflowing data are silently discarded by Write (--> exception in WriteBuffer!)
  //             Descendant class may override the OverFlow function and store the data elsewhere???
  
  ESubStreamOverflow=class(Exception);

  TSubStream=class(TStream)
  private
    FReadOnly: Boolean;
    FWarn: Boolean;
  protected
    FMaster: TStream;
    FPosition: integer;
    FOffset,FSize: integer;
    function Overflow(const Buffer; Count: Longint): Longint; virtual;
  public
    constructor Create(Master: TStream; Offset, Size: integer);
    destructor Destroy; override;
    property  Master: TStream read FMaster;
    function  Read(var Buffer; Count: Longint): Longint; override;
    function  Write(const Buffer; Count: Longint): Longint; override;
    function  Seek(Offset: Longint; Origin: Word): Longint; override;

    property  ReadOnly: Boolean read FReadOnly write FReadOnly default True;
    property  WarnOverflow: Boolean read FWarn write FWarn default True;
  end;

  {
  //---------------------------------------------------------
  // TStringStreamEx - more optimized TStringStream:
  TStringStreamEx=class(TStringStream)
  public
    function Write(const Buffer; Count: Longint): Longint; override;
  end;  }

type
  TTempFileStream=class(TFileStream)
  private
    FFileName: string;
  public
    constructor Create;
    destructor Destroy; override;
    property  FileName: string read FFileName;
  end;

  // Stream used to safely replace some file...
  // Writes to temporary file... If Commit is called, the original file is replaced at Destroy.
  // If Destroy is called before Commit (for ex. from finally in case of exception), temp-stream
  // is deleted and original file is left untouched...
  // TempFile is formated:   FileNameBase.~Ext  and overwritten without being tested for existence!
  // If cannot apply the change in Destructor, exception is raised!
  TReplaceStream=class(TFileStream)
  private
    OriFile,TempFile: string;
    FCommited,FHaveOriginal: Boolean;
  public
    constructor Create(const FileName: string);
    constructor CreateCopy(const FileName: string; Dummy: integer=0);
    destructor Destroy; override;
    procedure Commit;
    property  FileName: string read OriFile;  
  end;

  //--------------------------------------------------------------------
  // TStreamLineReader - read any stream line-by-line
  //                   - effective buffering
  //                   - UnGetLine & PeekLine
  //                   - stream doesn't have to be seekable
  //                   - is NOT thread-safe... use other locking mechanism to lock the stream...
  //                   - no-one else should access the stream!
  TStreamLineReader=class(TObject)
  private
    Lines:     TStringList;  // next few lines in a stream...
    FLastTrail,              // ... last line in buffer is not complete...
    FEOF,
    FRealEOF:  Boolean;
    FStream:   TStream;
    FPosition: integer;
    FRealPos:  integer;
  public
    constructor Create(InStream: TStream);
    {$IFDEF WIN32}destructor  Destroy; override;    {$ENDIF}
    function    ReadLine: string;               // get next line from stream...
    procedure   UnGetLine(const Line: string);  // give a line back to the logical head of a stream...
    function    PeekLine: string;               // get next line without removing it from the logical head of a stream...
  // Properties:
    property    EOF: Boolean read FEOF;
    property    Position: integer read FPosition;
    property    RealPos: integer read FRealPos;
  end;

type
  // ICompUnknown is a generic interface type for components, that don't live with
  // their interface Ref-Count... They may be free'd by component ownership,
  // while there still exists the interface...
  // If you store it to some variable, call variable.AddReference(@variable);
  // When the object is destroyed, it will set variable:=nil, if it still points to it...
  ICompUnknown=interface(IUnknown)
    ['{76CF6A60-8192-11D4-A01C-0050DADC2D85}']
    procedure AddReference(PIntf: Pointer);
  end;

  TCompUnknownCreateProc=function(Owner: TComponent; const IID: TGUID; out Obj: ICompUnknown): Boolean; 

  // ICompIntfList is a safe way to store ICompUnknown interfaces, which may be destroyed
  // while the list still exists...
  // As implemented below in TCompIntfList, you can also use List as ISomeInterface
  // to get some of it's items (first of that kind)...
  ICompIntfList=interface(ICompUnknown)
    ['{CB88AB60-A10F-11D4-A01C-0050DADC2D85}']
    // Locking:
    procedure Lock;
    procedure Unlock;
    // Manipulation:    !Add Dynamic appended to end!
    function Add(const IID: TGUID; const Obj: ICompUnknown): integer; overload; // Add with IID (for GetIntf)...
    function Add(const Obj: ICompUnknown): integer; overload; // Add anonymous...
    function Remove(const Obj: ICompUnknown): integer;
    function IndexOfObject(const Obj: ICompUnknown): Integer;
    function IndexOf(const IID: TGUID): integer;
    function Merge(Source: ICompIntfList): Boolean;
    // Retrieval by type:
    function Search(const IID: TGUID; out Obj; AnonymousOnly: Boolean=True): Boolean; // Calls QueryInterface for each item...
    function GetIntf(const IID: TGUID; out Obj): Boolean; // Only those, that were added with this type...
    // Retrieval by index:
    function GetCount: integer;
    function Get(Index: integer): ICompUnknown;
    function GetIID(Index: integer; var IID: TGUID): Boolean; // Get registered ID... (Used in Merge)
    // Properties:
    property Count: Integer read GetCount;
    property Items[Index: Integer]: ICompUnknown read Get; default;
    // Adding dynamic objects:
    function AddDynamic(const IID: TGUID; CreateProc: TCompUnknownCreateProc): integer;
    function IsDynamic(Index: integer; var CreateProc: TCompUnknownCreateProc): Boolean;
  end;

procedure Add_Ref(var Refs: TDualList; Ptr: Pointer);
//procedure Clear_Refs(var Refs: TDualList);

{$IFDEF WIN32}
type
  TCompIntfList=class(TComponent,ICompIntfList)
  private
    Refs:          TDualList;
    FItems:        TStringList;
    {$IFDEF WIN32}
    FLock:         TRtlCriticalSection; //@002-
    {$ELSE}
    FLock:         LongWord;  //@002+
    {$ENDIF}
    procedure Pack;
    function  AddNamed(const Name: string; const Obj: ICompUnknown): integer;
    function  CreateItem(APcil: Pointer; const IID: TGUID; out Obj): Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    function  QueryInterface(const IID: TGUID; out Obj): HResult; override; stdcall;
    procedure Clear;
    { ICompUnknown: }
    procedure AddReference(PIntf: Pointer);
    { ICompIntfList: }
    // Locking:
    procedure Lock;
    procedure Unlock;
    // Manipulation:
    function  Add(const IID: TGUID; const Obj: ICompUnknown): integer; overload; // Add with IID (for GetIntf)...
    function  Add(const Obj: ICompUnknown): integer; overload; // Add anonymous...
    function  AddDynamic(const IID: TGUID; CreateProc: TCompUnknownCreateProc): integer; // Add dynamic...
    function  Remove(const Obj: ICompUnknown): integer;
    function  IndexOfObject(const Obj: ICompUnknown): Integer;
    function  IndexOf(const IID: TGUID): integer;
    function  Merge(Source: ICompIntfList): Boolean;
    // Retrieval by type:
    function  Search(const IID: TGUID; out Obj; AnonymousOnly: Boolean=True): Boolean; // Calls QueryInterface for each item...
    function  GetIntf(const IID: TGUID; out Obj): Boolean; // Only those, that were added with this type...
    // Retrieval by index:
    function  GetCount: integer;
    function  Get(Index: integer): ICompUnknown;
    function  GetIID(Index: integer; var IID: TGUID): Boolean; // Get registered ID... (Used in Merge)
    function  IsDynamic(Index: integer; var CreateProc: TCompUnknownCreateProc): Boolean;
  end;
{$ENDIF}
// Use LoadStrings instead of TStrings.LoadFromStream... Spares memory (but is a little slower - but not much)
// (LoadFromStream loads whole stream into a string, and breaks it into the lines)
// This reads in 8k buffers onto the stack and caches lines into a string-list... 
procedure LoadStrings(Strings: TStrings; Stream: TStream);

function CopyStream(Source,Dest: TStream; RewindSource: Boolean): integer; // Works with non-seakable streams also...
function CopyStreamEx(Source,Dest: TStream; RewindSource: Boolean; CopySize: integer): integer; // Works with non-seakable streams also (if you don't set RewindSource)...

type
  //--------------------------------------------------------------------
  // TEqPoolAllocator - Pool allocator, equal-size blocks
  // Can alloc many blocks and free them in one step...

  TFinalizeProc=procedure(Item: Pointer);

  TEqPoolAllocator=class(TObject)
  private
    FOnFinal: TFinalizeProc;
    procedure SetOnFinal(Value: TFinalizeProc);
  protected
    FreeList: TList;
    AllocList: TSortedList;
    Blocks: TDualList;
    FItemSize: integer;
    bnFinal: Boolean;
 //   procedure SetItemSize(Value: integer); virtual; // EXC, if not empty!
 //   function  AllocBlock(var BlockSize: integer): Pointer; virtual;
    procedure FreeBlock(Block: Pointer); virtual;
    function  InWhichBlock(Ptr: Pointer; Suggest: integer): integer; virtual;
    procedure DoFinalize(Item: Pointer);
  public
   // destructor Destroy; override;
   // property  ItemSize: integer read FItemSize write SetItemSize;
  //  function  AllocItem: Pointer;
  //  function  FreeItem(kit: Pointer): Boolean;
   {$IFDEF WIN32} procedure Clear; {$ENDIF}
  //  procedure CollectFree; // if some blocks are entirelly un-used, frees them...
    property  OnFinalizeItem: TFinalizeProc read FOnFinal write SetOnFinal;
  end;

{----------------------------------------------------------}
{ Hacks                                                    }
{----------------------------------------------------------}

type
  TToggleProc=procedure(Obj: TObject; SetIt: Boolean);
  TListScanProc=function(Obj: TObject; List,Arg: Pointer): Boolean;
  TListScanProcEx=function(Obj: TObject; List,Arg: Pointer; Offset: integer): Boolean;

// Find offset of private variable, if you know, how to toggle it:
// ToggleProc is called twice:  Once SetIt=True, Second time SetIt=False
// Object is snapped between the steps and compared at the end to find the byte, that changed...
// Beware of side effects, when toggling the property!
function GetPrivateOffset(Obj: TObject; ToggleProc: TToggleProc): integer;

// Find offset of private list, if you would recognize it:
// List is a pointer (a dw-round number)...
// Any pointer is passed to ScanProc, until it recognizes the list.
// (Often you know the type of the list (for ex. TList) and its items (for ex. TWinControl.Controls[i]), but don't know it's address...)
//function GetPrivateListOffset(Obj: TObject; ListScanProc: TListScanProc; Arg: Pointer): integer;
//function GetPrivateListOffsetEx(Obj: TObject; ListScanProcEx: TListScanProcEx; Arg: Pointer): integer;
// Applications:

//function GetStringListList(List: TStringList): PStringItemList;
function GetStringsUpdateCount(List: TStrings): integer;

{----------------------------------------------------------}
// Interface for general program usage...
// Should be an interfaced object! (beware of EXC. @ prog. done, if invalid pointer is here!)
// If this unit resides in a package, it can be accessed by all modules (DLLs), which share the package...
var AppGlobalDataStorage:IUnknown=nil;
{----------------------------------------------------------}
// Interface of application's main form:
// (Use "as" to get particular sub-objects...) May be NIL for apps, that don's use this feature !
// It is advised, that it supports ICompIntfList...
var AppMain:IUnknown=nil;
{$IFDEF WIN32}
  function GetCompList: ICompIntfList; // Returns (AppMain as ICompIntfList) or creates private list, which can AppMain merge later...
  procedure DropInternalCompList; // Only if (AppMain<>nil) and Supports(AppMain,ICompIntfList) !
{$ENDIF}
{----------------------------------------------------------}

resourcestring
  SDuplicateString='Dup string!';
  SListIndexError='List index out of bounds (%d)';
  SListCapacityError = 'List capacity out of bounds (%d)';
  SListCountError = 'List count out of bounds (%d)';
  SSortedListError='Sorted list err!';

implementation

uses       //@002+
  DbugIntf;    //@002+
  //  lclintf; //@002+-

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
{$IFDEF WIN32}//@002+
function ListRemove(var List: TList; Item: Pointer): integer;
begin
  Result:=-1;
  if List<>nil then begin
    Result:=List.Remove(Item);
    if (Result<=0) and (List.Count=0) then // if Result>0 then This_item_was_not_last_in_the_list!
      SaveFree(@List);
  end;
end;
{$ENDIF}//@002+
{$IFDEF WIN32}//@002+
procedure DestroyList(var List: TList); // with Objects...
var i: integer;
    obj: TObject;
begin
  if List<>nil then begin
    for i:=List.Count-1 downto 0 do try
      obj:=TObject(List[i]);   //@001=
      List.Delete(i);
      obj.Free;
    except ; end;
    SaveFree(@List);
  end;
end;
{$ENDIF}//@002+

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

{$IFDEF WIN32} //@002+
procedure FreeStrListAndItems(var Strings; Objects: Boolean); // with Objects~Pointers
var list: TStrings;
    obj: TObject;
    i: integer;
begin
  obj:=TObject(Strings);
  if (obj=nil) or not Is_Object(obj) then exit;
  list:=obj as TStrings;
  TObject(Strings):=nil;
  for i:=list.Count-1 downto 0 do begin
    obj:=list.Objects[i];
    list.Delete(i);
    if Objects then
      obj.Free
    else
      Freemem(Pointer(obj));
  end;
  list.Free;
end;
{$ENDIF}//@002+

{$IFDEF WIN32}//@002+
procedure ListFreeByType(var List: TList; Cls: TClass);
var i: integer;
    obj: TObject;
begin
  if List<>nil then begin
    for i:=List.Count-1 downto 0 do try
      obj:=TObject(List[i]);           //@001=
      if (obj<>nil) and not (obj is Cls) then continue;
      List.Delete(i);
      obj.Free;
    except ; end;
    if List.Count=0 then SaveFree(@List);
  end;
end;
{$ENDIF}//@002+
procedure AssignList(dst,src: TList);
begin
  if dst=nil then exit;
  dst.Clear;
  if src=nil then exit;
  dst.Count:=src.Count;
  Move(src.List^,dst.List^,4*src.Count);
end;

function GetSerial(var LastValue: integer): integer;  assembler; //@001=
asm //                 EAX                ->EAX
  mov  edx,eax
  mov  eax,1
  lock xadd [edx],eax
end;

{$ifndef VER130}
procedure FreeAndNil(var Obj);
var Instance: TObject;
begin
  Instance:=TObject(Obj);
  TObject(Obj):=nil;
  Instance.Free;
end;
{$endif}

function ClassIs(Have,Want: TClass): Boolean;
begin
  while (Have<>nil) and (Have<>Want) do
    Have:=Have.ClassParent;
  Result:=(Have=Want);
end;

{$IFDEF WIN32}//@002+
function Is_Object(Obj: TObject): Boolean;
type PClass=^TClass;
begin
  Result:=(Obj<>nil) and IsClass(PClass(Obj)^);
end;
{$ENDIF}//@002+

{$IFDEF WIN32}//@002+
function IsClass(Cls: TClass): LongBool;
type PPChar=^PChar;
var pname: PChar;
    par: TClass;
const clsMax=40; // maxim·lnû—dÈlka n·zvu tû—dy:
begin
  Result:=False;
  try
    // Ovû—Ìme, ûe vû—ec lze ËÌst VMT:
    if (integer(Cls)<$400000) or IsBadReadPtr(PChar(Cls)+vmtIntfTable,-vmtIntfTable) then exit;
    if IsBadCodePtr(PChar(Cls)+vmtDestroy) then exit;
    // N·zev tû—dy objektu:
    pname:=PPChar(PChar(Cls)+vmtClassName)^;
    if (integer(pname)<$400000) or {also covering <0 (ie. >=$80000000) !}
       IsBadReadPtr(pname,4)
    then exit;
    if (not ((pname+1)^ in ['T','t'])) or  // Object class-name doesn't start with "T" !
       (ord(pname^)>clsMax)                // Class-name too long !
    then exit;
    par:=Cls.ClassParent;
    // Parent class:
    if par=nil then begin
      // Class has no parent... Only TObject should have no parent!
      Result:=(StrIComp(pname+1,'TObject')=0);
      exit;
    end;
    //!SpeedUp! if IsBadReadPtr(PChar(par)+vmtIntfTable,-vmtIntfTable) then exit;
    // Parent class-name:
    pname:=PPChar(PChar(par)+vmtClassName)^;
    //!SpeedUp! if IsBadReadPtr(pname,4) then exit;
    if (integer(pname)<$400000) or
       (not ((pname+1)^ in ['T','t'])) or  // Object class-name doesn't start with "T" !
       (ord(pname^)>clsMax)                // Class-name too long !
    then exit;
  except Result:=False; exit; end;
  // OK:
  Result:=True;
end;
{$ENDIF}//@002+

{$ifdef TOHLE_TO_ASI_TAK_DELA:}
procedure SaveFree(pobj: pointer);
type PObject=^TObject;
var obj: TObject;
begin
  if pobj=nil then exit;
  obj:=PObject(pobj)^;
  PObject(pobj)^:=nil;
  if (obj<>nil) and (IsClass(obj.ClassType)) then obj.Free;
end;
{$else -> CHYTREJSI_ZPUSOB}

{$IFDEF WIN32} //@002+
procedure SaveFree(pobj: pointer); assembler;
asm
  or eax,eax ; jz @done // if pobj=nil then exit;
  mov ecx,[eax]         // obj:=PObject(pobj)^;
  xor edx,edx
  mov [eax],edx         // PObject(pobj)^:=nil;
  mov eax,ecx           // if obj=nil then exit;
  test eax,eax
  je @done
  // Verify valid address:
  js  @CheckObjAddress // >=$80000000
  // on NT it is possible, that a memory is allocated in range 00100000-00400000 !
  // so don't refuse low address ...
  cmp eax,$00400000
  jbe  @CheckObjAddress // <=$40000000
@AddressOk:
  // Ovû—enû—ned·vnû—od-alokace:
  mov ecx,[eax]  // ECX = VMT
  cmp ecx,3 ; je @Error // zmena proti TObject.Free: Prevent message:   Access violation.... read of address FFFFFFFF
  // Ovû—enû— ûe je to skuteËnû—TObject:
  push ebx
  push ecx
  mov  ebx,eax   // EBX = object
  mov  eax,ecx   // EAX = VMT
  call IsClass
  test eax,eax   // tested later:
  mov  eax,ebx   // EAX = object
  pop  ecx       // ECX = VMT
  pop  ebx
  jz   @Error    // flag from   test eax,eax   before
@free:
  MOV     DL,1
  CALL    dword ptr [ECX].vmtDestroy
  jmp     @done
@CheckObjAddress:
  push    eax  // save object ptr
  // if IsBadWritePtr(Obj,4) then Error!
  push    4
  push    eax
  call    Windows.IsBadWritePtr
  test    eax,eax
  pop     eax // restore object ptr...
  jz      @AddressOk
@Error:
  //call bad-handler:
  mov     edx,[esp]-4
  //mov     &ExceptAddr,edx
  mov     ecx,BadUnalloc
  test    ecx,ecx
  jz      @done
  call    ecx
@done:
end;
{$ENDIF} //@002+
{$endif}

procedure SaveFreeMem(pptr: pointer);
type PPointer=^Pointer;
var ptr: Pointer;
begin
  if pptr=nil then exit;
  ptr:=PPointer(pptr)^;
  PPointer(pptr)^:=nil;
  if ptr<>nil then FreeMem(ptr);
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

function PeekToken(const Line: string; sep: char): string;
var pln: PChar;
begin
  Result:='';
  pln:=PChar(Line);
  if pln=nil then exit;
  Result:=pExtractToken(pln,sep);
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

// This returns @(#0) if not found #10...
function FastEOL(str: PChar): PChar; assembler;
asm //             EAX->ESI
  push esi
  mov esi,eax
  mov edx,$00000D0A        // DL=$0A  DH=$0D
  or esi,esi ; jz @ret_esi
@fe_cy:
  lodsb
  or al,al ; je @ret_esi_m1
  cmp al,dl ; je @ret_esi_m1
  cmp al,dh ; jne @fe_cy
@ret_esi_m1:
  dec esi
@ret_esi:
  mov eax,esi
  pop esi
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

function IcIndexOf(List: TStrings; const Item: string): integer;
var i: integer;
begin
  if List<>nil then
    for i:=0 to List.Count-1 do
      if AnsiCompareText(List[i],Item)=0 then begin
        Result:=i;
        exit;
      end;
  Result:=-1;
end;

{$IFDEF WIN32}//@002+
procedure InitColor;
var DC: HDC;
begin
  DC:=GetDC(0);
  if DC<>0 then
    try
      BitsPerPixel:=GetDeviceCaps(DC,BITSPIXEL);
      Lo_Color:=(BitsPerPixel<=8);
    finally ReleaseDC(0,DC); end
  else Lo_Color:=True;
end;
{$ENDIF}//@002+

{$IFDEF WIN32}//@002+
function Is256Color: Boolean;
begin
  if BitsPerPixel=0 then InitColor;
  Result:=Lo_Color;
end;
{$ENDIF}//@002+

var
  DeleteList:TStringList=nil;

procedure NeedDeleteList;
begin
  if DeleteList=nil then begin
    DeleteList:=TStringList.Create;
    DeleteList.Sorted:=True;
    DeleteList.Duplicates:=dupIgnore;
  end;
end;

procedure FinalDeleteFile(const FileName: string);
begin
  NeedDeleteList;
  DeleteList.Add(ExpandFileName(FileName));
end;

procedure FinalDeleteFiles(List: TStrings);
begin
  if List<>nil then begin
    NeedDeleteList;
    DeleteList.AddStrings(List);
  end;
end;

procedure RemoveFinalDelete(const FileName: string);
var idx: integer;
begin
  if (DeleteList<>nil) and DeleteList.Find(ExpandFileName(FileName),idx) then begin
    DeleteList.Delete(idx);
    if DeleteList.Count=0 then FreeAndNil(DeleteList);
  end;
end;

procedure DeleteFiles(List: TStrings; bnCanFinal: Boolean);
var i: integer;
    FileName: string;
begin
  if List=nil then exit;
  if List=DeleteList then bnCanFinal:=False;
  for i:=List.Count-1 downto 0 do begin
    FileName:=List[i];
    List.Delete(i);
    if not DeleteFile(FileName) and bnCanFinal and FileExists(FileName) then
      FinalDeleteFile(FileName);
  end;
end;

{----------------------------------------------------------}
{ Special lists:                                           }
{----------------------------------------------------------}

{ TDualStringList }

destructor TDualStringList.Destroy;
begin
  FOnChange := nil;
  FOnChanging := nil;
  inherited Destroy;
  if FCount <> 0 then Finalize(FList^[0], FCount);
  FCount := 0;
  SetCapacity(0);
end;

procedure TDualStringList.ErrorRes(Msg: PResStringRec; Data: Integer);
begin
  Error(LoadResString(Msg),Data);
end;

function TDualStringList.Add(const S: string): Integer;
begin
  if not Sorted then
    Result := FCount
  else
    if Find(S, Result) then
      case Duplicates of
        dupIgnore: Exit;
        dupError: ErrorRes(@SDuplicateString, 0);
      end;
  InsertItem(Result, S);
end;

procedure TDualStringList.Changed;
begin
  if (GetStringsUpdateCount(Self) <= 0) and Assigned(FOnChange) then
    FOnChange(Self);
end;

procedure TDualStringList.Changing;
begin
  if (GetStringsUpdateCount(Self) <= 0) and Assigned(FOnChanging) then
    FOnChanging(Self);
end;

procedure TDualStringList.Clear;
begin
  if FCount <> 0 then
  begin
    Changing;
    Finalize(FList^[0], FCount);
    FCount := 0;
    SetCapacity(0);
    Changed;
  end;
end;

procedure TDualStringList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Changing;
  Finalize(FList^[Index]);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index],
      (FCount - Index) * SizeOf(T2StringItem));
  Changed;
end;

procedure TDualStringList.Exchange(Index1, Index2: Integer);
begin
  if (Index1 < 0) or (Index1 >= FCount) then ErrorRes(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then ErrorRes(@SListIndexError, Index2);
  Changing;
  ExchangeItems(Index1, Index2);
  Changed;
end;

procedure TDualStringList.ExchangeItems(Index1, Index2: Integer);
var
  Temp: Integer;
  Item1, Item2: P2StringItem;
begin
  Item1 := @FList^[Index1];
  Item2 := @FList^[Index2];
  Temp := Integer(Item1^.Name);
  Integer(Item1^.Name) := Integer(Item2^.Name);
  Integer(Item2^.Name) := Temp;
  Temp := Integer(Item1^.Value);
  Integer(Item1^.Value) := Integer(Item2^.Value);
  Integer(Item2^.Value) := Temp;
end;

function TDualStringList.Find(const S: string; var Index: Integer): Boolean;
var
  L, H, I, C: Integer;
begin
  Result := False;
  L := 0;
  H := FCount - 1;
  while L <= H do
  begin
    I := (L + H) shr 1;
    C := AnsiCompareText(FList^[I].Name, S);
    if C < 0 then L := I + 1 else
    begin
      H := I - 1;
      if C = 0 then
      begin
        Result := True;
        if Duplicates <> dupAccept then L := I;
      end;
    end;
  end;
  Index := L;
end;

function TDualStringList.Get(Index: Integer): string;
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Result := FList^[Index].Name;
end;

function TDualStringList.GetCapacity: Integer;
begin
  Result := FCapacity;
end;

function TDualStringList.GetCount: Integer;
begin
  Result := FCount;
end;

function TDualStringList.GetObject(Index: Integer): TObject;
begin
  {if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Result := FList^[Index].FObject;{}
  Result:=nil;
end;

procedure TDualStringList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then Delta := FCapacity div 4 else
    if FCapacity > 8 then Delta := 16 else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TDualStringList.IndexOf(const S: string): Integer;
begin
  if not Sorted then Result := inherited IndexOf(S)
  else
    if not Find(S, Result) then Result := -1;
end;

function TDualStringList.IndexOfValue(const S: string): Integer;
var i: integer;
begin
  for i:=0 to Count-1 do begin
    if AnsiCompareText(S,FList[i].Value)=0 then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

function TDualStringList.IcIndexOf(const S: string): Integer;
var i: integer;
begin
  if Sorted then begin
    // Find is case-insensitive...
    if not Find(S, Result) then Result := -1;
  end else begin
    Result:=-1;
    for i:=0 to Count-1 do
      if AnsiCompareText(FList[i].Name,S)=0 then begin
        Result:=i;
        exit;
      end;
  end;
end;

procedure TDualStringList.Insert(Index: Integer; const S: string);
begin
  if Sorted then ErrorRes(@SSortedListError, 0);
  if (Index < 0) or (Index > FCount) then ErrorRes(@SListIndexError, Index);
  InsertItem(Index, S);
end;

procedure TDualStringList.InsertItem(Index: Integer; const S: string);
begin
  Changing;
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1],
      (FCount - Index) * SizeOf(T2StringItem));
  with FList^[Index] do
  begin
    Pointer(Name) := nil;
    Pointer(Value) := nil;
    Name := S;
  end;
  Inc(FCount);
  Changed;
end;

procedure TDualStringList.Put(Index: Integer; const S: string);
begin
  if Sorted then ErrorRes(@SSortedListError, 0);
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Changing;
  FList^[Index].Name := S;
  Changed;
end;

procedure TDualStringList.PutObject(Index: Integer; AObject: TObject);
begin
  {if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Changing;
  FList^[Index].FObject := AObject;
  Changed;{}
end;

procedure TDualStringList.QuickSort(L, R: Integer; SCompare: TDualStringListSortCompare);
var
  I, J, P: Integer;
begin
  repeat
    I := L;
    J := R;
    P := (L + R) shr 1;
    repeat
      while SCompare(Self, I, P) < 0 do Inc(I);
      while SCompare(Self, J, P) > 0 do Dec(J);
      if I <= J then
      begin
        ExchangeItems(I, J);
        if P = I then
          P := J
        else if P = J then
          P := I;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then QuickSort(L, J, SCompare);
    L := I;
  until I >= R;
end;

procedure TDualStringList.SetCapacity(NewCapacity: Integer);
begin
  if NewCapacity<=FCount then exit;
  ReallocMem(FList, NewCapacity * SizeOf(T2StringItem));
  {if NewCapacity>FCount then
    FillChar(FList[FCount],(NewCapacity-FCount)*sizeof(T2StringItem),0); // Init new memory to 0's !
  {}
  FCapacity := NewCapacity;
end;

procedure TDualStringList.SetSorted(Value: Boolean);
begin
  if FSorted <> Value then
  begin
    if Value then Sort;
    FSorted := Value;
  end;
end;

procedure TDualStringList.SetUpdateState(Updating: Boolean);
begin
  if Updating then Changing else Changed;
end;

function StringListAnsiCompare(List: TDualStringList; Index1, Index2: Integer): Integer;
begin
  Result := AnsiCompareText(List.FList^[Index1].Name,
                            List.FList^[Index2].Name);
end;

procedure TDualStringList.Sort;
begin
  CustomSort(StringListAnsiCompare);
end;

procedure TDualStringList.CustomSort(Compare: TDualStringListSortCompare);
begin
  if not Sorted and (FCount > 1) then
  begin
    Changing;
    QuickSort(0, FCount - 1, Compare);
    Changed;
  end;
end;

function TDualStringList.GetValue(Index: integer): string;
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Result := FList^[Index].Value;
end;

procedure TDualStringList.PutValue(Index: integer; const Value: string);
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Changing;
  FList^[Index].Value := Value;
  Changed;
end;

function TDualStringList.AddValue(const Name, Value: string): Integer;
begin
  Changing;
  Result:=Add(Name);
  PutValue(Result,Value);
  Changed;
end;

procedure TDualStringList.InsertValue(Index: integer; const Name, Value: string);
begin
  Changing;
  Insert(Index, Name);
  PutValue(Index, Value);
  Changed;
end;

function TDualStringList.GetUpdateCount: integer;
begin
  Result:=GetStringsUpdateCount(Self);
end;

function TDualStringList.GetValueN(const Name: string): string;
var idx: integer;
begin
  Result:='';
  idx:=IcIndexOf(Name);
  if idx>=0 then
    Result:=FList[idx].Value;
end;

procedure TDualStringList.SetValueN(const Name, Value: string);
var idx: integer;
begin
  idx:=IcIndexOf(Name);
  if idx>=0 then begin
    Changing;
    FList[idx].Value:=Value;
    Changed;
  end else
    AddValue(Name,Value);
end;

procedure TDualStringList.SwapNamesAndValues; assembler;
asm
  mov edx,[eax].TDualStringList.FList
  mov ecx,[eax].TDualStringList.FCount
  jecxz @none
@cy:
  mov  eax,[edx]
  xchg eax,[edx+4]
  mov  [edx],eax
  add  edx,8
  loop @cy
@none:
end;

procedure TDualStringList.Assign(Source: TPersistent);
var i: integer;
    psrc,pdst: P2StringItem;
    Cnt: integer;
begin
  if Source=nil then exit;
  if (Source is TDualStringList) then begin
    BeginUpdate;
    try
      Clear;
      with TDualStringList(Source) do begin
        Cnt:=Count;
        if Cnt=0 then exit;
        psrc:=@FList[0];
      end;
      SetCapacity(Cnt); // memory is NOT initialized!   All valid items were finalized by Clear above...
      FCount:=Cnt;
      pdst:=@Self.FList[0];
      for i:=0 to Cnt-1 do begin
        Pointer(pdst^.Name):=nil;
        pdst^.Name:=psrc^.Name;
        Pointer(pdst^.Value):=nil;
        pdst^.Value:=psrc^.Value;
        inc(pdst);
        inc(psrc);
      end;
    finally EndUpdate; end;
  end else
    inherited;
end;

function TDualStringList.GetIniText: string;
var i,len: integer;
    pit: P2StringItem;
    pdst: PChar;
begin
  len:=0;
  // Count result size:
  pit:=@FList[0];
  for i:=0 to Count-1 do begin
    inc(len,Length(pit^.Name)+Length(pit^.Value)+3);
    inc(pit);
  end;
  // Collect:
  SetLength(Result,len);
  if len=0 then exit;
  pdst:=@Result[1];
  pit:=@FList[0];
  for i:=0 to Count-1 do begin
    len:=Length(pit^.Name);
    if len>0 then begin
      System.Move(Pointer(pit^.Name)^,pdst^,len);
      inc(pdst,len);
    end;
    pdst^:='='; inc(pdst);
    len:=Length(pit^.Value);
    if len>0 then begin
      System.Move(Pointer(pit^.Value)^,pdst^,len);
      inc(pdst,len);
    end;
    pdst^:=#13; inc(pdst);
    pdst^:=#10; inc(pdst);
    inc(pit);
  end;
end;

procedure TDualStringList.SetIniText(const Value: string);
var pln: PChar;
    Line,Name: string;
begin
  BeginUpdate;
  try
    Clear;
    pln:=PChar(Value);
    if pln<>nil then
      while {(pln<>nil) and{} (pln^<>#0) do begin
        Line:=ExtractLine(pln);
        Name:=Trim(ExtractToken(Line,'='));
        AddValue(Name,TrimRight(Line));
      end;
  finally EndUpdate; end;
end;

{----------------------------------------------------------}
{ Hacks                                                    }
{----------------------------------------------------------}

function GetPrivateOffset(Obj: TObject; ToggleProc: TToggleProc): integer;
var Snap: Pointer;
    Size: integer;
begin
  Size:=Obj.InstanceSize;
  GetMem(Snap,Size);
  try
    ToggleProc(Obj,True);
    System.Move(Pointer(Obj)^,Snap^,Size);
    ToggleProc(Obj,False);
    // Find the changed byte:
    asm
      push esi ; push edi
      mov ecx,&Size
      mov edx,ecx   // EDX saves size...
      mov esi,&Snap
      mov eax,&Obj // EAX saves Obj
      mov edi,eax
      inc eax // instead of dec edi later...
      repe cmpsb
      //dec edi // inc eax was done before...
      sub edi,eax // EDI=distance from Obj
      cmp edi,edx // if not found, it equals the InstanceSize
      jb @found
      xor edi,edi // return 0, if not found a difference...
    @found:
      mov &Result,edi
      pop edi ; pop esi
    end;
  finally FreeMem(Snap); end;
end;
{$IFDEF WIN32}  //@002+
function GetPrivateListOffset(Obj: TObject; ListScanProc: TListScanProc; Arg: Pointer): integer;
var pi: PInteger;
    i,Count,List: integer;
begin
  pi:=Pointer(Obj);
  Count:=Obj.InstanceSize div 4;
  inc(pi); dec(Count); // don't scan VMT...
  for i:=0 to Count-1 do begin
    List:=pi^;
    if (List<>0) // cannot test contents of empty list...
       and ((List and 3)=0) // all allocated pointers & objects should be dword aligned...
       and not IsBadReadPtr(Pointer(List),4) // and it must point into the address space...
       and ListScanProc(Obj,Pointer(List),Arg) // ? caller recognized it's list...
    then begin
      Result:=integer(pi)-integer(Obj);
      exit;
    end;
    inc(pi);  // Step by 4... Let's assume, that objects are compiled with dword alignment! If not, may not find the list...
  end;
  Result:=0;
end;
{$ENDIF} //@002+

{$IFDEF WIN32}//@002+
function GetPrivateListOffsetEx(Obj: TObject; ListScanProcEx: TListScanProcEx; Arg: Pointer): integer;
var pi: PInteger;
    i,Count,List,offs: integer;
begin
  pi:=Pointer(Obj);
  Count:=Obj.InstanceSize div 4;
  inc(pi); dec(Count); // don't scan VMT...
  for i:=0 to Count-1 do begin
    List:=pi^;
    offs:=integer(pi)-integer(Obj);
    if (List<>0) // cannot test contents of empty list...
       and ((List and 3)=0) // all allocated pointers & objects should be dword aligned...
       and not IsBadReadPtr(Pointer(List),4) // and it must point into the address space...
       and ListScanProcEx(Obj,Pointer(List),Arg,offs) // ? caller recognized it's list...
    then begin
      Result:=offs; //integer(pi)-integer(Obj);
      exit;
    end;
    inc(pi);  // Step by 4... Let's assume, that objects are compiled with dword alignment! If not, may not find the list...
  end;
  Result:=0;
end;
{$ENDIF} //@002+

const
  SList_List_Offset:integer=0;
  SList_UpCnt_Offset:integer=0;

function Is_AB_ItemList(Obj: TObject; ItList,Arg: Pointer): Boolean;
type PPChar=^PChar;
var pit: PPChar;
    item: PChar;
begin
  Result:=False;
  pit:=ItList;
  item:=pit^;
  if (item=nil) or not (item^='A') then exit; // First string...
  inc(pit);
  if pit^<>nil then exit; // First object should be nil...
  inc(pit);
  item:=pit^;
  if (item=nil) or not (item^='B') then exit; // Second string...
  {inc(pit);
  if pit^<>nil then exit;{} // Second object should be nil...
  Result:=True;
end;

{
function GetStringListList(List: TStringList): PStringItemList;
var offs: integer;
    Measure: TStringList;
begin
  offs:=SList_List_Offset;
  if offs=0 then begin
    Measure:=TStringList.Create;
    Measure.Add('A'); Measure.Add('B');  // 2 items, which we will test...
    SList_List_Offset:=GetPrivateListOffset(Measure,Is_AB_ItemList,nil);
    offs:=SList_List_Offset;
  end;
  Result:=PStringItemList(PInteger(integer(List)+offs)^);
end;  }

procedure ToggleUpdateCount(Obj: TObject; SetIt: Boolean);
begin
  if SetIt then
    TStrings(Obj).BeginUpdate
  else
    TStrings(Obj).EndUpdate;
end;

function GetStringsUpdateCount(List: TStrings): integer;
var offs: integer;
    Measure: TStringList;
begin
  offs:=SList_UpCnt_Offset;
  if offs=0 then begin
    Measure:=TStringList.Create;
    SList_UpCnt_Offset:=GetPrivateOffset(Measure,ToggleUpdateCount);
    Measure.Free;
    offs:=SList_UpCnt_Offset;
  end;
  Result:=PInteger(integer(List)+offs)^;
end;

{ TNamedInterfaceList }
 {
function TNamedInterfaceList.AddIntf(const Name: string; const Intf: IUnknown): integer;
begin
  Result:=Add(Name);
  PutIntf(Result,Intf);
end;  }
{
procedure TNamedInterfaceList.Clear;
var Items: PInterfaceItemArray;
begin
  if Count<>0 then begin
    Items:=Pointer(GetStringListList(Self));
    Finalize(Items[0],Count);
    inherited;
  end;
end;  }
{
procedure TNamedInterfaceList.Delete(Index: Integer);
var Items: PInterfaceItemArray;
begin
  if (Index>=0) and (Index<Count) then begin
    Items:=Pointer(GetStringListList(Self));
    Finalize(Items[Index]);
  end;
  inherited;
end;   }
  {
destructor TNamedInterfaceList.Destroy;
var Items: PInterfaceItemArray;
begin
  if Count<>0 then begin
    Items:=Pointer(GetStringListList(Self));
    Finalize(Items[0],Count);
  end;
  inherited;
end; }
     {
function TNamedInterfaceList.GetIntf(Index: integer): IUnknown;
var Items: PInterfaceItemArray;
begin
  Result:=nil;
  if (Index>0) and (Index<=Count) then begin
    Items:=Pointer(GetStringListList(Self));
    Result:=Items[Index].FIntf;
  end;
end;
      }
function TNamedInterfaceList.GetObject(Index: Integer): TObject;
begin
  Result:=nil;
end;
       {
procedure TNamedInterfaceList.InsertIntf(Index: integer; const Name: string; const Intf: IUnknown);
var Items: PInterfaceItemArray;
begin
  Insert(Index,Name);
  Items:=Pointer(GetStringListList(Self));
  Items[Index].FIntf:=Intf;
end;    }
  {
procedure TNamedInterfaceList.Move(CurIndex, NewIndex: Integer);
var
  TempIntf: IUnknown;
  TempString: string;
begin
  if CurIndex <> NewIndex then
  begin
    BeginUpdate;
    try
      TempString := Get(CurIndex);
      TempIntf := GetIntf(CurIndex);
      Delete(CurIndex);
      InsertIntf(NewIndex, TempString, TempIntf);
    finally
      EndUpdate;
    end;
  end;
end;  }
     {
procedure TNamedInterfaceList.PutIntf(Index: integer; const Value: IUnknown);
var Items: PInterfaceItemArray;
begin
  if (Index>=0) and (Index<Count) then begin
    Items:=Pointer(GetStringListList(Self));
    Items[Index].FIntf:=Value;
  end;
end;    }

procedure TNamedInterfaceList.PutObject(Index: Integer; AObject: TObject);
begin
  // do nothing!   (no exception - AddStrings,Assign etc. should work...)
end;

{ TDualList }

function TDualList.Add(Item: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result].Item := Item;
  FList^[Result].Value := nil; //Later bug correction:   !Prevent undefined values!
  Inc(FCount);
end;

function TDualList.AddIiValue(Item, Value: integer): integer; assembler;
asm
  jmp TDualList.AddValue
end;

function TDualList.AddIpValue(Item: integer; Value: Pointer): integer; assembler;
asm
  jmp TDualList.AddValue
end;

function TDualList.AddPiValue(Item: Pointer; Value: integer): integer; assembler;
asm
  jmp TDualList.AddValue
end;

function TDualList.AddValue(Item, Value: Pointer): Integer;
begin
  Result := FCount;
  if Result = FCapacity then
    Grow;
  FList^[Result].Item := Item;
  FList^[Result].Value := Value;
  Inc(FCount);
end;

procedure TDualList.Clear;
begin
  SetCount(0);
  SetCapacity(0);
end;

procedure TDualList.Delete(Index: Integer);
begin
  if (Index < 0) or (Index >= FCount) then
    ErrorRes(@SListIndexError, Index);
  Dec(FCount);
  if Index < FCount then
    System.Move(FList^[Index + 1], FList^[Index], (FCount - Index) * SizeOf(TDualListItem));
end;

destructor TDualList.Destroy;
begin
  Clear;
end;

class procedure TDualList.ErrorRes(Msg: PResStringRec; Data: Integer);
begin
  {TList.{}Error(LoadResString(Msg), Data);
end;

class procedure TDualList.Error(const Msg: string; Data: Integer);

  function ReturnAddr: Pointer;
  asm
          MOV     EAX,[EBP+4]
  end;

begin
  raise EListError.CreateFmt(Msg, [Data]) at ReturnAddr;
end;

procedure TDualList.Exchange(Index1, Index2: Integer);
var
  Item,Value: Pointer;
begin
  if (Index1 < 0) or (Index1 >= FCount) then ErrorRes(@SListIndexError, Index1);
  if (Index2 < 0) or (Index2 >= FCount) then ErrorRes(@SListIndexError, Index2);
  Item := FList^[Index1].Item;
  Value := FList^[Index1].Value;
  FList^[Index1].Item := FList^[Index2].Item;
  FList^[Index1].Value := FList^[Index2].Value;
  FList^[Index2].Item := Item;
  FList^[Index2].Value := Value;
end;

function TDualList.Expand: TDualList;
begin
  if FCount = FCapacity then
    Grow;
  Result := Self;
end;

function TDualList.First: Pointer;
begin
  Result := Get(0);
end;

function TDualList.GetId(Index: integer): integer; assembler;
asm
  jmp TDualList.Get
end;

function TDualList.Get(Index: Integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Result := FList^[Index].Item;
end;

function TDualList.GetIValue(Index: Integer): integer; assembler;
asm
  jmp TDualList.GetValue
end;

function TDualList.GetValue(Index: integer): Pointer;
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  Result := FList^[Index].Value;
end;

procedure TDualList.PutId(Index: integer; Value: integer); assembler;
asm
  jmp TDualList.Put
end;

procedure TDualList.Put(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  FList^[Index].Item := Item;
end;

procedure TDualList.PutIValue(Index: Integer; Value: integer); assembler;
asm
  jmp TDualList.PutValue
end;

procedure TDualList.PutValue(Index: Integer; Value: Pointer);
begin
  if (Index < 0) or (Index >= FCount) then ErrorRes(@SListIndexError, Index);
  FList^[Index].Value := Value;
end;

procedure TDualList.Grow;
var
  Delta: Integer;
begin
  if FCapacity > 64 then
    Delta := FCapacity div 4
  else
    if FCapacity > 8 then
      Delta := 16
    else
      Delta := 4;
  SetCapacity(FCapacity + Delta);
end;

function TDualList.IndexOf(Item: Pointer): Integer;
var List: PDualListItemArray;
begin
  Result := 0;
  List := FList;
  while (Result < FCount) and (List^[Result].Item <> Item) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

function TDualList.IndexOfValue(Value: Pointer): Integer;
var List: PDualListItemArray;
begin
  Result := 0;
  List := FList;
  while (Result < FCount) and (List^[Result].Value <> Value) do
    Inc(Result);
  if Result = FCount then
    Result := -1;
end;

procedure TDualList.Insert(Index: Integer; Item: Pointer);
begin
  if (Index < 0) or (Index > FCount) then ErrorRes(@SListIndexError, Index);
  if FCount = FCapacity then Grow;
  if Index < FCount then
    System.Move(FList^[Index], FList^[Index + 1], (FCount - Index) * SizeOf(TDualListItem));
  FList^[Index].Item := Item;
  FList^[Index].Value := nil; //Later bug correction:   !Prevent undefined values!
  Inc(FCount);
end;

procedure TDualList.InsertValue(Index: Integer; Item, Value: Pointer);
begin
  Insert(Index,Item);
  //PutValue(Index,Value);
  FList^[Index].Value := Value; // Later optimization...
end;

function TDualList.Last: Pointer;
begin
  Result := Get(FCount - 1);
end;

procedure TDualList.Move(CurIndex, NewIndex: Integer);
var
  Item,Value: Pointer;
begin
  if CurIndex <> NewIndex then begin
    if (NewIndex < 0) or (NewIndex >= FCount) then ErrorRes(@SListIndexError, NewIndex);
    Item := Get(CurIndex); // Can raise exc...
    Value := FList^[CurIndex].Value;
    FList^[CurIndex].Item := nil;
    FList^[CurIndex].Value := nil; // Later correction...
    Delete(CurIndex);
    Insert(NewIndex, nil);
    FList^[NewIndex].Item := Item;
    FList^[NewIndex].Value := Value;
  end;
end;

procedure TDualList.Pack;
var
  I: Integer;
begin
  for I := FCount - 1 downto 0 do
    with FList^[I] do
      if (Item=nil) and (Value=nil) then
        Delete(I);
end;

function TDualList.Remove(Item: Pointer): Integer;
begin
  try                                            //@002+
    Result := IndexOf(Item);
    if Result >= 0 then
      Delete(Result);

  except                                         //@002+
    SendDebug('TDualList.Remove: Error');        //@002+
    Result := -1;                                //@002+
  end;                                           //@002+
end;

function TDualList.RemoveValue(Value: Pointer): Integer;
begin
  Result := IndexOfValue(Value);
  if Result >= 0 then
    Delete(Result);
end;
                           
procedure TDualList.SetCapacity(NewCapacity: Integer);
begin
  if (NewCapacity < FCount) or (NewCapacity > MaxListSize) then ErrorRes(@SListCapacityError, NewCapacity);
  if NewCapacity <> FCapacity then
  begin
    ReallocMem(FList, NewCapacity * SizeOf(TDualListItem));
    FCapacity := NewCapacity;
  end;
end;

procedure TDualList.SetCount(NewCount: Integer);
var
  I: Integer;
begin
  if (NewCount < 0) or (NewCount > MaxListSize) then ErrorRes(@SListCountError, NewCount);
  if NewCount > FCapacity then
    SetCapacity(NewCount);
  if NewCount > FCount then
    FillChar(FList^[FCount], (NewCount - FCount) * SizeOf(TDualListItem), 0)
  else
    for I := FCount - 1 downto NewCount do
      Delete(I);
  FCount := NewCount;
end;

procedure QuickSort2(SortList: PDualListItemArray; L, R: Integer; SCompare: TListSortCompare);
var
  I, J: Integer;
  P, T, TV: Pointer;
begin
  repeat
    I := L;
    J := R;
    P := SortList^[(L + R) shr 1].Item;
    repeat
      while SCompare(SortList^[I].Item, P) < 0 do
        Inc(I);
      while SCompare(SortList^[J].Item, P) > 0 do
        Dec(J);
      if I <= J then
      begin
        T := SortList^[I].Item;
        TV := SortList^[I].Value;
        SortList^[I].Item := SortList^[J].Item;
        SortList^[I].Value := SortList^[J].Value;
        SortList^[J].Item := T;
        SortList^[J].Value := TV;
        Inc(I);
        Dec(J);
      end;
    until I > J;
    if L < J then
      QuickSort2(SortList, L, J, SCompare);
    L := I;
  until I >= R;
end;

function AscendSortCompare(Item1,Item2: Pointer): integer;
begin
  Result:=integer(Item1)-integer(Item2);
end;

procedure TDualList.Sort(Compare: TListSortCompare);
begin
  if not Assigned(Compare) then
    Compare:=@AscendSortCompare;
  if (FList <> nil) and (Count > 0) then
    QuickSort2(FList, 0, Count - 1, Compare);
end;

{ TSubStrings }

var
  SubMasters:TDualList=nil;
  {$IFDEF WIN32}
  SubMasterLock:TRTLCriticalSection;     //@002=
  {$ELSE}
  SubMasterLock:LongWord;
  {$ENDIF}
  SubMasterLock_Init:Boolean;
{$IFDEF WIN32}
procedure Lock_Sm(Lock: Boolean);
begin
  if Lock then
    EnterCriticalSection(SubMasterLock)
  else
    LeaveCriticalSection(SubMasterLock);
end;
{$ENDIF}
 {
function AddSubStrings(Master: TStrings; Sub: TSubStrings): TList;
var idx: integer;
begin
  Lock_Sm(True);
  try
    if SubMasters=nil then begin
      SubMasters:=TDualList.Create;
      idx:=-1;
    end else
      idx:=SubMasters.IndexOf(Master);
    
    if idx>=0 then begin
      Result:=SubMasters.Values[idx];
    end else begin
      Result:=TList.Create;
      SubMasters.AddValue(Master,Result);
    end;
    Result.Add(Sub);
  finally Lock_Sm(False); end;
end;  }

{$IFDEF WIN32}//@002+
procedure RemoveSubStrings(Master: TStrings; Sub: TSubStrings);
var idx: integer;
    List: TList;
begin
  Lock_Sm(True);
  try
    if SubMasters=nil then exit;
    idx:=SubMasters.IndexOf(Master);
    if idx>=0 then begin
      List:=SubMasters.Values[idx];
      List.Remove(Sub);
      if List.Count=0 then begin
        SubMasters.Delete(idx);
        List.Free;
        if SubMasters.Count=0 then begin
          SaveFree(@SubMasters);
        end;
      end;
    end;
  finally Lock_Sm(False); end;
end;
{$ENDIF}//@002+
  {
function SubStringsCount(Master: TStrings): integer;
var idx: integer;
begin
  Result:=0;
  Lock_Sm(True);
  try
    if SubMasters<>nil then begin
      idx:=SubMasters.IndexOf(Master);
      if idx>=0 then
        Result:=TList(SubMasters.Values[idx]).Count; 
    end;
  finally Lock_Sm(False); end;
end;    }
  {
procedure DestroySubStrings(Master: TStrings);
var List: TList;
    i,idx: integer;
begin
  Lock_Sm(True);
  try
    if SubMasters<>nil then begin
      idx:=SubMasters.IndexOf(Master);
      if idx>=0 then begin
        List:=SubMasters.Values[idx];
        for i:=List.Count-1 downto 0 do
          TObject(List[i]).Free; // it will remove itself from the list...
      end;
    end;
  finally Lock_Sm(False); end;
end;  }

function GetSubsMaster(Source: TStrings; var Offset: integer): TStrings;
begin
  Result:=Source;
  Offset:=0;
  while (Result<>nil) and (Result is TSubStrings) do begin
    inc(Offset,TSubStrings(Result).FFirst);
    Result:=TSubStrings(Result).Master;
  end;
end;

{}
{constructor TSubStrings.Create(Master: TStrings; FromIndex, ToIndex: integer);
Label _Invalid;
begin
  if not SubMasterLock_Init then begin
    InitializeCriticalSection(SubMasterLock);
    SubMasterLock_Init:=True;
  end;

  inherited Create;
  
  if (Master=nil) or (FromIndex<0) then begin
  _Invalid:
    raise EInvalidOperation.Create('Invalid master-list or indexes for a sub-string-list...');
  end;
  if ToIndex<0 then begin
    // Until current end of Master:
    ToIndex:=Master.Count;
    if FromIndex>ToIndex then goto _Invalid;
  end else
    if (ToIndex>Master.Count) or (FromIndex>ToIndex) then
      goto _Invalid;
  FMaster:=Master;
  FFirst:=FromIndex;
  FLast:=ToIndex;
  FColleagues:=AddSubStrings(Master,Self);
end;    }

{$IFDEF WIN32}//@002+
destructor TSubStrings.Destroy;
begin
  RemoveSubStrings(Master,Self);
  inherited;
end;
{$ENDIF}//@002+

procedure TSubStrings.Notification(Reason: TssNotification; Index1, Index2: integer);
begin
  // Other sub-strings did something to the master:
  case Reason of
    ssnInsert: begin
      if Index1<=FLast then begin
        inc(FLast);
        if Index1<=FFirst then inc(FFirst);
      end;
    end;
    ssnMove: begin
      Notification(ssnDelete,Index1,0);
      Notification(ssnInsert,Index2,0);
    end;
    ssnDelete: begin
      if Index1<FLast then begin
        dec(FLast);
        if Index1<FFirst then dec(FFirst)
        else if FLast<FFirst then FLast:=FFirst; //???? this should not happen!
      end;
    end;
  end;
end;

procedure TSubStrings.Notify(Reason: TssNotification; Index1, Index2: integer);
var i: integer;
    Other: TSubStrings;
begin
  if Colleagues.Count>1 then begin
    inc(Index1,FFirst);
    inc(Index2,FFirst);
    for i:=0 to Colleagues.Count-1 do begin
      Other:=Colleagues[i];
      if Other<>Self then
        Other.Notification(Reason,Index1,Index2);
    end;
  end;
end;
{
procedure TSubStrings.Lock;
begin
  Lock_Sm(True);
end;  }

{procedure TSubStrings.UnLock;
begin
  Lock_Sm(False);
end;   }

procedure TSubStrings.IndexError(Index: integer);
begin
  Error('List index out of bounds %d',Index);
end;

procedure TSubStrings.ReadOnlyError;
begin
  Error('SubStrings are read-only!',0);
end;

procedure TSubStrings.SetFirst(Value: integer);
begin
  if (Value<0) or (Value>=FMaster.Count) then IndexError(Value);
  FFirst:=Value;
end;

procedure TSubStrings.SetLast(Value: integer);
begin
  if (Value<FFirst) or (Value>FMaster.Count) then IndexError(Value);
  FLast:=Value;
end;
{
procedure TSubStrings.Clear;
var i: integer;
begin
  if ReadOnly then ReadOnlyError;
  Lock_Sm(True);
  try
    BeginUpdate;
    try
      for i:=GetCount-1 downto 0 do
        DoDelete(i);
    finally EndUpdate; end;
  finally Lock_Sm(False); end;
end;  }

procedure TSubStrings.DoDelete(Index: integer);
begin
  FMaster.Delete(Index+FFirst);
  dec(FLast);
  Notify(ssnDelete,Index,0);
end;

{procedure TSubStrings.Delete(Index: Integer);
begin
  if ReadOnly then ReadOnlyError;
  inc(Index,FFirst);
  if (Index<0) or (Index>=FLast) then IndexError(Index-FFirst);
  Lock_Sm(True);
  try
    DoDelete(Index);
  finally Lock_Sm(False); end;
end;   }

{procedure TSubStrings.Insert(Index: Integer; const S: string);
begin
  if ReadOnly then ReadOnlyError;
  Lock_Sm(True);
  try
    FMaster.Insert(Index+FFirst,S);
    inc(FLast);
    Notify(ssnInsert,Index,0);
  finally Lock_Sm(False); end;
end;   }

{procedure TSubStrings.Move(CurIndex, NewIndex: Integer);
var mci,mni,cnt: integer;
begin
  if ReadOnly then ReadOnlyError;
  Lock_Sm(True);
  try
    cnt:=GetCount;
    if (CurIndex<0) or (CurIndex>=cnt) then IndexError(CurIndex);
    if (NewIndex<0) or (NewIndex>=cnt) then IndexError(NewIndex);
    mci:=CurIndex+FFirst;
    mni:=NewIndex+FFirst;
    FMaster.Move(mci,mni);
    Notify(ssnMove,CurIndex,NewIndex); // all but self...
    //Notification(ssnMove,mci,mni); // notify self also...   Not needed... items would not cross our boundaries and no shift occurs...
  finally Lock_Sm(False); end;
end;   }

function TSubStrings.GetCount: Integer;
begin
  Result:=FLast-FFirst;
end;

procedure TSubStrings.Exchange(Index1, Index2: Integer);
begin
  if ReadOnly then ReadOnlyError;
  FMaster.Exchange(Index1,Index2);
end;

function TSubStrings.Get(Index: Integer): string;
begin
  Result:=FMaster[Index+FFirst];
end;

function TSubStrings.GetObject(Index: Integer): TObject;
begin
  Result:=FMaster.Objects[Index+FFirst];
end;

procedure TSubStrings.Put(Index: Integer; const S: string);
begin
  if ReadOnly then ReadOnlyError;
  FMaster[Index]:=S;
end;

procedure TSubStrings.PutObject(Index: Integer; AObject: TObject);
begin
  if ReadOnly then ReadOnlyError;
  FMaster.Objects[Index]:=AObject;
end;

{procedure TSubStrings.Assign(Source: TPersistent);
var i,st_add,midx,Count,SrcCount: integer;
    Src: TStrings;
begin
  if ReadOnly then ReadOnlyError;
  if Source=nil then begin
    Clear;
    exit;
  end;
  if Source is TStrings then begin
    Src:=TStrings(Source);
    Lock_Sm(True);
    try
      Count:=Self.Count;
      midx:=FFirst; // master-index
      // Replace master lines:  (conventional way: Clear + Add(all) is not effective with SubStrings...)
      st_add:=-1;
      SrcCount:=Src.Count;
      for i:=0 to SrcCount-1 do begin
        if i>=Count then begin
          // rest of the lines would be added...
          st_add:=i;
          break;
        end;
        FMaster[midx]:=Src[i];
        FMaster.Objects[midx]:=Src.Objects[i];
        inc(midx);
      end;
      if SrcCount>Count then begin
        // Add next lines:
        for i:=st_add to SrcCount-1 do
          Self.AddObject(Src[i],Src.Objects[i]);
      end else if SrcCount<Count then begin
        // Delete rest of lines:
        for i:=Count-1 downto SrcCount do
          Self.DoDelete(i);
      end;
    finally Lock_Sm(False); end;
  end else
    inherited; // mostly an exception...
end;      }

{ TSubStream }

constructor TSubStream.Create(Master: TStream; Offset, Size: integer);
Label _Invalid;
var msz: integer;
begin
  // Verify params:
  if (Master=nil) then begin
  _Invalid:
    raise EInvalidOperation.Create('Invalid sub-stream operation!');
  end;
  if Offset<0 then Offset:=Master.Position;
  msz:=Master.Size;
  if Size<0 then Size:=msz-Offset else
  if Offset+Size>msz then goto _Invalid;
  // Create:
  inherited Create;
  // Set values:
  FMaster:=Master;
  FOffset:=Offset;
  FSize:=Size;
  FReadOnly:=True;
  FWarn:=True;
end;

destructor TSubStream.Destroy;
begin
  inherited;
end;

function TSubStream.Seek(Offset: Integer; Origin: Word): Longint;
begin
  case Origin of
    soFromBeginning: FPosition:=Offset;
    soFromCurrent: inc(FPosition,Offset);
    soFromEnd: FPosition:=FSize+Offset;
  end;
  if FPosition<0 then FPosition:=0
  else if FPosition>FSize then FPosition:=FSize;
  Result:=FPosition;
end;

function TSubStream.Read(var Buffer; Count: Integer): Longint;
begin
  // Seek master stream:
  Master.Seek(FOffset+FPosition,soFromBeginning);
  // Adjust count:
  if FPosition+Count>FSize then Count:=FSize-FPosition;
  if Count<0 then begin
    Result:=0;
    exit;
  end;
  // Read:
  Result:=Master.Read(Buffer,Count);
  // Skip read part:
  inc(FPosition,Result); // FPosition:=FMaster.Position-FOffset;
end;

function TSubStream.Write(const Buffer; Count: Integer): Longint;
begin
  if FReadOnly then raise Exception.Create('Sub-Stream is read-only!');
  // Seek master stream:
  Master.Seek(FOffset+FPosition,soFromBeginning);
  // Verify the count:
  if FPosition+Count>FSize then begin
    Result:=Overflow(Buffer,Count);
    exit;
  end;
  // Write:
  Result:=Master.Write(Buffer,Count);
  // Skip written part:
  inc(FPosition,Result); // FPosition:=FMaster.Position-FOffset;
end;

function TSubStream.Overflow(const Buffer; Count: Integer): Longint;
begin
  if FPosition+Count>FSize then begin
    if FWarn then
      raise ESubStreamOverflow.Create('Sub-stream overflow!');
    Count:=FSize-FPosition;
    if Count<0 then begin
      Result:=0;
      exit;
    end;
  end;
  // Write only the amount, that fits...
  Result:=Write(Buffer,Count);
end;

{ TStringStreamEx }

// Accessing TStringStream private fields (FDataString and FPosition) :

var
  TStrStm_isz:integer=0;
  TStm_isz:integer=0;

procedure Init_Sstm_Sz;
begin
  TStrStm_isz:=TStringStream.InstanceSize;
  TStm_isz:=TStream.InstanceSize;
end;

procedure SStm_Error;
begin
  raise Exception.Create('TStringStream access error!');
end;

{ //@002-
function GetSStmPtr(Obj: TStringStreamEx; Value: integer): Pointer; assembler;
asm //              EAX                   EDX
  push  edi
  push  esi
  mov   edi,eax
  mov   esi,edx
  mov   ecx,TStrStm_isz
  test  ecx,ecx
  jnz   @sz_known
  call  Init_Sstm_Sz  // Init InstanceSizes...
  mov   ecx,TStrStm_isz
@sz_known:
  // Search only fields, added in TStringStream (ignore anything in TStream and it's class-parents...)
  sub   ecx,TStm_isz
  add   edi,TStm_isz
  shr   ecx,2 // searching dwords...
  jecxz @Abort
  mov   eax,esi
  repne scasd
  sub   edi,4 // undo last step
  mov   eax,edi // result is a pointer to value... (let's assume that the value would be found...)
  pop   esi
  pop   edi
  ret
@Abort:
  call  SStm_Error
end;
  }//@002-
{ //@002-
function TStringStreamEx.Write(const Buffer; Count: Integer): Longint;
var dlen: integer;
    ppos: ^Integer;
    ps: ^string;
    pc: PChar;
begin
  if Count=0 then begin
    Result:=0;
    exit;
  end;
  dlen:=Length(DataString);
  if (Position=dlen) or (DataString='') then begin
    // Append to stream (original version with re-allocation) :
    Result:=inherited Write(Buffer,Count);
  end else begin
    ppos:=GetSStmPtr(Self,Position);
    ps:=GetSStmPtr(Self,integer(DataString));
    Result:=Count;
    if ppos^+Result>dlen then begin
      // Re-alloc the DataString:
      dlen:=ppos^+Result; // new length of DataString...
      SetLength(ps^,dlen);
    end;
    UniqueString(ps^);
    pc:=PChar(Pointer(ps^))+ppos^;
    Move(Buffer, pc^, Result);
    Inc(ppos^, Result);
  end;
end;
 }//@002-
{ TStreamLineReader }

constructor TStreamLineReader.Create(InStream: TStream);
begin
  FStream:=InStream;
  Lines:=TStringList.Create;
  if InStream=nil then begin
    FEOF:=True;
    FRealEOF:=True;
  end;
end;

{$IFDEF WIN32}//@002+
destructor TStreamLineReader.Destroy;
begin
  SaveFree(@Lines);
  inherited;
end;
{$ENDIF}//@002+

const SRL_BUFSIZE=8192;

function TStreamLineReader.ReadLine: string;
var buf: array[0..SRL_BUFSIZE] of char;
    pc,pbe: PChar;
    sz,last: integer;
    Line: string;
Label _GiveLine,_Restart,_NextChunk;

  function GetNextLine(pc: PChar): PChar;
  var pe: PChar;
  Label _Again;
  begin
    pe:=FastEOL(pc);   // stops on some of #13,#10,#0
  _Again:
    FLastTrail:=(pe=pbe);
    if (not FLastTrail) and (pe^=#0) then begin
      pe:=FastEOL(pe+1); // get over #0...
      goto _Again;
    end;
    SetString(Line,pc,pe-pc);
    if pe^=#13 then inc(pe);
    if pe^=#10 then inc(pe);
    Result:=pe;
    //Result:=SkipEOL(pe);
  end;

begin
  if FEOF then begin
    Result:='';
    exit;
  end;
  _Restart:
  if Lines.Count>1 then begin // last line in a cache may not be complete... At least 2 lines must be cached to return from cache!
    // Give some cached line:
    _GiveLine:
    Result:=Lines[0];
    Lines.Delete(0);
    inc(FPosition,length(Result)+2);
    exit;
  end;
  // No cached lines:
  if FRealEOF then begin
    FEOF:=True;
    // in-stream's EOF...
    if Lines.Count=1 then begin
      // give last line (even if not complete):
      goto _GiveLine;
    end;
    Result:='';
    exit;
  end;
  // Read next chunk into buffer (on stack) :
  _NextChunk:
  sz:=FStream.Read(buf[0],sizeof(buf)-1);
  pbe:=@buf[sz]; pbe^:=#0; //buf[sz]:=#0;
  inc(FRealPos,sz);
  if sz=0 then begin  
    FRealEOF:=True; //if sz<sizeof(buf)-1 then FRealEOF:=True;
    goto _Restart; // give last incomplete line... Nothing more in a stream...
  end; //!!! if sz<sizeof(buf)-1 then Mostly_Sure_FRealEOF:=True;
  pc:=@buf[0];
  if FLastTrail then begin
    // Get trailing part of last line:
    pc:=GetNextLine(pc);
    last:=Lines.Count-1;
    Lines[last]:=Lines[last] + Line; // append to last incomplete line...
    if FLastTrail then goto _NextChunk; // line still not complete... must read another 8k...
  end;
  // Parse other lines in buffer:
  while pc<pbe do begin
    pc:=GetNextLine(pc);
    Lines.Add(Line);
  end;
  goto _Restart; // give first line in a buffer...
end;

procedure TStreamLineReader.UnGetLine(const Line: string);
begin
  Lines.Insert(0,Line);
  dec(FPosition,length(Line)-2);
  FEOF:=False;
end;

function TStreamLineReader.PeekLine: string;
begin
  if not EOF then begin
    Result:=ReadLine;
    UngetLine(Result);
  end else Result:='';
end;

procedure LoadStrings(Strings: TStrings; Stream: TStream);
var srl: TStreamLineReader;
    capa: integer;
begin
  if Strings=nil then exit;
  Strings.Clear;
  if Stream=nil then exit;
  
  capa:=0;
  try
    // Capacity estimation... if stream is not seekable, exception...
    // if it succeeds --> less re-allocations of Strings...
    capa:=(Stream.Size-Stream.Position) div 40; // say there would be 40 chars per line...
    Strings.Capacity:=capa;
    capa:=Strings.Capacity; // TStringList rounds-up...
  except ; end;

  Strings.BeginUpdate;
  srl:=nil;
  try
    srl:=TStreamLineReader.Create(Stream);
    while not srl.EOF do
      Strings.Add(srl.ReadLine);
  finally
    srl.Free;
    Strings.EndUpdate;
  end;

  try
    // It seems we overestimated the line-count:
    if (Strings.Capacity=capa) and (capa>Strings.Count+16) then
      Strings.Capacity:=Strings.Count;
  except ; end;
end;


{ TSubFiller }
{
constructor TSubFiller.Create(Master: TStrings; FromIndex, ToIndex: integer);
begin
  Inherited Create(Master,FromIndex,FromIndex);
  if (ToIndex<FromIndex) or (ToIndex>Master.Count) then
    raise EInvalidOperation.CreateFmt('SubFiller: ToIndex too small! (From=%d,To=%d,Master.Count=%d)',[FromIndex,ToIndex,Master.Count]); 
  FCeiling:=ToIndex;
end;     }
{
destructor TSubFiller.Destroy;
begin
  Cut;
  inherited;
end;   }

{procedure TSubFiller.Cut;
var i,del_base,del_cnt: integer;
begin
  Lock_Sm(True);
  try
    if FLast>=FCeiling then exit;
    del_base:=FLast;
    del_cnt:=FCeiling-FLast; // delete items [FLast]...[FCeiling-1]
    for i:=del_cnt+del_base-1 downto del_base do begin
      FMaster.Delete(i);
      dec(FCeiling);
      Notify(ssnDelete,i,0);
    end;
  finally Lock_Sm(False); end;
end;         }

{function TSubFiller.Add(const S: string): Integer;
begin
  Lock_Sm(True);
  try
    if FLast<FCeiling then begin
      inc(FLast);
      Result:=FLast-FFirst-1;
      Put(Result,S);
      if InitObjects then
        PutObject(Result,nil);
    end else begin
      Result:=Count;
      Insert(Result,S);
    end;
  finally Lock_Sm(False); end;
end;   }

{procedure TSubFiller.Insert(Index: Integer; const S: string);
var cnt: integer;
begin
  Lock_Sm(True);
  try
    cnt:=GetCount;
    if (Index=cnt) then begin
      // This is actually an Add...
      if (FLast>=FCeiling) then begin
        inc(FCeiling); // Self not notified...
        inherited Insert(Index,S); // Shift+Notification...
        exit;
      end else begin
        Add(S); // our Add doesn't recycle into Insert always...
        exit;
      end;
    end;
    if Index<cnt then
      raise EInvalidOperation.Create('Cannot insert into middle of sub-filler!');
  finally Lock_Sm(False); end;
end;  }

procedure TSubFiller.Clear;
begin
  FLast:=FFirst;
end;

{procedure TSubFiller.Delete(Index: Integer);
var cnt: integer;
begin
  Lock_Sm(True);
  try
    cnt:=GetCount;
    if Index=cnt-1 then begin
      Dec(FLast);
    end else begin
      if (Index<0) or (Index>=cnt) then IndexError(Index);
      dec(FCeiling); // Self not notified...
      DoDelete(Index);
    end;
  finally Lock_Sm(False); end;
end;   }

procedure TSubFiller.Notification(Reason: TssNotification; Index1, Index2: integer);
begin
  inherited;
  case Reason of
    ssnInsert: if Index1<=FCeiling then inc(FCeiling);
    ssnDelete: if Index1< FCeiling then dec(FCeiling);
  end;
end;

{ TTempFileStream }

function GetTempFile(const prefix: string): string;
var path,pref3: string;
    ppref: PChar;
const sDefPrefix='dfs';
begin
  SetLength(path,255);
//  SetLength(path,GetTempPath(255,@path[1]));
  SetLength(path,Length(GetTempDir(False)));
  SetLength(Result,255);
  Result[1]:=#0;
  case length(prefix) of
    0: ppref:=PChar(sDefPrefix);
    1,2: begin
      pref3:=prefix;
      while length(pref3)<3 do pref3:=pref3+'_';
      ppref:=PChar(pref3);
    end;
    3: ppref:=PChar(prefix);
    else begin
      pref3:=Copy(prefix,1,3);
      ppref:=PChar(pref3);
    end;
  end;
  GetTempFileName(PChar(path),ppref,0,PChar(Result));
  SetLength(Result,StrLen(PChar(Result)));
end;

constructor TTempFileStream.Create;
begin
  FFileName:=GetTempFile(''); // Windows.GetTempFileName creates the file...
  inherited Create(FFileName,fmOpenReadWrite or fmShareDenyWrite);
end;

destructor TTempFileStream.Destroy;
begin
  try
    inherited;
  finally DeleteFile(FFileName); end;
end;

{ TSortedList }

function TSortedList.Add(Item: Pointer): Integer;
begin
  if Find(Item,Result) and not Duplicates then begin
    Result:=-1;
    exit;
  end;
  InsertItem(Result,Item);
end;

function TSortedList.AddItem(Item: Pointer): Integer;
begin
  Result:=inherited Add(Item);
end;

function DefCompare(P1,P2: Pointer): integer;
begin
  Result:=integer(P1)-integer(P2);
end;

{$ifndef VER130}
type
  TListHack=class(TObject)
    FList: PPointerList;
  end;
{$endif not VER130}

type
  TFindInfo=record
    Comp:  TListSortCompare;
    List:  PPointerList;
    Count: integer;
    Dups:  Boolean;
  end;

function SortedFind(var Info: TFindInfo; Item: Pointer; var Index: integer): Boolean;
var L, H, I, C: Integer;
begin
  Result := False;
  with Info do begin
    L := 0;
    H := Count - 1;
    while L <= H do
    begin
      I := (L + H) shr 1;
      C := Comp(List[I], Item);
      if C < 0 then L := I + 1 else
      begin
        H := I - 1;
        if C = 0 then
        begin
          Result := True;
          //if Duplicates <> dupAccept then L := I;
          if not Dups then L := I;
        end;
      end;
    end;
    Index := L;
  end;
end;

function TSortedList.Find(Item: Pointer; var Index: integer): Boolean;
var Info: TFindInfo;
begin
  Info.Comp:=Self.Compare;
  if @Info.Comp=nil then Info.Comp:=@DefCompare;
  Info.List:=Self.List;
  //    Info.List:=TListHack(Self).FList; //@002-
  Info.Count:=Self.Count;
  Info.Dups:=Self.Duplicates;
  Result:=SortedFind(Info,Item,Index);
end;

procedure TSortedList.Insert(Index: Integer; Item: Pointer);
begin
  raise EInvalidOperation.Create('Cannot insert into a sorted list!');
end;

procedure TSortedList.InsertItem(Index: integer; Item: Pointer);
begin
  inherited Insert(Index,Item);
end;

procedure TSortedList.Move(CurIndex, NewIndex: Integer);
begin
  raise EInvalidOperation.Create('Cannot move in a sorted list!');
end;

procedure TSortedList.MoveItem(CurIndex, NewIndex: Integer);
begin
  inherited Move(CurIndex, NewIndex);
end;

procedure TSortedList.SetCompare(const Value: TListSortCompare);
begin
  if @Value=@FCompare then exit;
  FCompare:=Value;
  if Assigned(Value) then Sort(Value);
end;

procedure TSortedList.Assign(OtherList: TList);
var Comp: TListSortCompare;
begin
  // Move items:
  Self.Capacity:=OtherList.Count;
  System.Move(OtherList.List[0],Self.List[0],OtherList.Count*4);
  // Sort:
  Comp:=Self.Compare;
  if @Comp=nil then Comp:=@DefCompare;
  Sort(Comp);
end;

procedure SListAdd(var List: TSortedList; Item: Pointer);
begin
  if List=nil then List:=TSortedList.Create;
  List.Add(Item);
end;

{$IFDEF WIN32}//@002+
function SListRemove(var List: TSortedList; Item: Pointer): integer;
var idx: integer;
begin
  Result:=-1;
  if List<>nil then begin
    if List.Find(Item,idx) then begin
      Result:=idx;
      List.Delete(idx);
    end;
    if List.Count=0 then SaveFree(@List);
  end;
end;
{$ENDIF}//@002+
{ TEqPoolAllocator }

{
destructor TEqPoolAllocator.Destroy;
begin
  Clear;
  inherited;
end;  }
 {
procedure TEqPoolAllocator.SetItemSize(Value: integer);
begin
  if (Blocks<>nil) and (Blocks.Count>0) then begin
    if (AllocList=nil) or (AllocList.Count=0) then
      Clear
    else
      raise Exception.Create('Cannot change item size!');
  end;
  FItemSize:=Value;
end;    }
{$IFDEF WIN32}//@002+
procedure TEqPoolAllocator.Clear;
var i: integer;
    block: Pointer;
begin
  SaveFree(@FreeList);
  if bnFinal and (AllocList<>nil) then
    try
      for i:=0 to AllocList.Count-1 do
        FOnFinal(AllocList[i]);
    except ; end;
  SaveFree(@AllocList);
  if Blocks<>nil then begin
    for i:=Blocks.Count-1 downto 0 do begin
      block:=Blocks[i];
      Blocks.Delete(i);
      FreeBlock(block);
    end;
    SaveFree(@Blocks);
  end;
end;
{$ENDIF}//@002+
{
function TEqPoolAllocator.AllocBlock(var BlockSize: integer): Pointer;
var Size: integer;
begin
  if Blocks=nil then Blocks:=TDualList.Create;
  Size:=ItemSize*128;
  BlockSize:=Size;
  Result:=AllocMem(Size);
  Blocks.AddValue(Result,Pointer(Size));
end;     }

procedure TEqPoolAllocator.FreeBlock(Block: Pointer);
begin
  // Block is not in the Blocks list any more! No need to remove it...
  FreeMem(Block);
end;
   {
function TEqPoolAllocator.AllocItem: Pointer;
var i,itsz,BlockSize,ItemCount: integer;
    block: PChar;
    frls: TList;
Label _HaveResult;
begin
  if (FreeList<>nil) then begin
    i:=FreeList.Count-1;
    if (i>=0) then begin
      Result:=FreeList.List[i];
      FreeList.Delete(i);
      goto _HaveResult;
    end;
  end;
  // Alloc new block:
  block:=AllocBlock(BlockSize);
  itsz:=Self.ItemSize;
  ItemCount:=BlockSize div itsz;
  // Prevent re-allocations of FreeList.Items:
  if Self.FreeList=nil then begin
    Self.FreeList:=TList.Create;
    frls:=Self.FreeList;
    frls.Capacity:=ItemCount;
  end else
    frls:=Self.FreeList;
    if frls.Capacity-frls.Count<ItemCount then
      frls.Capacity:=frls.Capacity+ItemCount;
  // Divide new block:    Add all-but-last to the FreeList
  for i:=1 to ItemCount-1 do begin
    frls.Add(block);
    inc(block,itsz);
  end;
  // Return last item:
  Result:=block;
_HaveResult:
  SListAdd(AllocList,Result);
  FillChar(Result^,ItemSize,0);
end;
  }
{
function TEqPoolAllocator.FreeItem(kit: Pointer): Boolean;
begin
  Result:=(SListRemove(AllocList,kit)>=0);
  if Result then begin
    DoFinalize(kit); //if bnFinal then FOnFinal(kit);
    ListAdd(FreeList,kit);
  end;
end;    }

procedure TEqPoolAllocator.SetOnFinal(Value: TFinalizeProc);
begin
  FOnFinal:=Value;
  bnFinal:=Assigned(Value);
end;

procedure TEqPoolAllocator.DoFinalize(Item: Pointer);
begin
  if bnFinal then
    FOnFinal(Item);
end;

function TEqPoolAllocator.InWhichBlock(Ptr: Pointer; Suggest: integer): integer;
var block: Pointer;
    i,size: integer;
begin
  Result:=-1;
  if Blocks=nil then exit; // not probable...
  // Look at suggested block:
  if (Suggest>=0) and (Suggest<Blocks.Count-1) then begin
    block:=Blocks[Suggest];
    if integer(Ptr)>=integer(block) then begin
      size:=integer(Blocks.Values[Suggest]);
      if integer(Ptr)<integer(block)+Size then begin
        Result:=Suggest;
        exit;
      end;
    end;
  end;
  // All blocks:
  for i:=0 to Blocks.Count-1 do begin
    block:=Blocks[i];
    if integer(Ptr)>=integer(block) then begin
      size:=integer(Blocks.Values[i]);
      if integer(Ptr)<integer(block)+Size then begin
        Result:=i;
        exit;
      end;
    end;
  end;
end;

{ Slowish way:

procedure TEqPoolAllocator.CollectFree;
var i,lastbl,bl,blCnt: integer;
    used: TBits;
    item: Pointer;
begin
  if (AllocList=nil) or (AllocList.Count=0) or (Blocks=nil) or (Blocks.Count=0) then begin
    Clear;
    exit;
  end;
  used:=TBits.Create;
  try
    blCnt:=Blocks.Count;
    used.Size:=blCnt;
    lastbl:=-1;
    for i:=0 to AllocList.Count-1 do begin
      item:=AllocList[i];
      bl:=InWhichBlock(item,lastbl);
      if bl=lastbl then continue; // mostly (AllocList.Count-Blocks.Count times) true...
      if bl=-1 then continue; // not much probable (anything we alloc resides in some block)...
      lastbl:=bl;
      used[bl]:=True;
      if used.OpenBit=blCnt then break; // all blocks are used...
    end;
    for i:=blCnt-1 downto 0 do
      if not used[i] then begin
        block:=Blocks[i];
        Blocks.Delete(i);
        FreeBlock(Block);
      end;
  finally used.Free; end;
end;{}
{
procedure TEqPoolAllocator.CollectFree;
var i,idx: integer;
    block: Pointer;
Label _FreeBl;
begin
  if (AllocList=nil) or (AllocList.Count=0) or (Blocks=nil) or (Blocks.Count=0) then begin
    Clear;
    exit;
  end;
  for i:=Blocks.Count-1 downto 0 do begin
    block:=Blocks[i];
    AllocList.Find(block,idx); //... AllocList is sorted... if there is something from the block, found index would point on it...
    if (idx=AllocList.Count) then goto _FreeBl;
    if InWhichBlock(AllocList[idx],i)=i then
      continue;
  _FreeBl:
    Blocks.Delete(i);
    FreeBlock(block);
  end;
end;
    }
function CopyStream(Source,Dest: TStream; RewindSource: Boolean): integer; // Works with non-seakable streams also...
var buf: array[0..(32*1024)-1] of byte;
    sz: integer;
begin
  Result:=0;
  if (Source=nil) then exit;
  if RewindSource then Source.Position:=0;
  if (Dest=nil) then exit;
  while True do begin
    sz:=Source.Read(buf[0],SizeOf(buf));
    if sz<=0 then break;
    Dest.Write(buf[0],sz);
    inc(Result,sz);
  end;
end;

function CopyStreamEx(Source,Dest: TStream; RewindSource: Boolean; CopySize: integer): integer; // Works with non-seakable streams also (if you don't set RewindSource)...
var buf: array[0..(32*1024)-1] of byte;
    sz: integer;
begin
  Result:=0;
  if (Source=nil) then exit;
  if RewindSource then Source.Position:=0;
  if (Dest=nil) then exit;
  while CopySize>0 do begin
    sz:=SizeOf(buf);
    if sz>CopySize then sz:=CopySize;
    sz:=Source.Read(buf[0],sz);
    if sz<=0 then break;
    Dest.Write(buf[0],sz);
    dec(CopySize,sz);
    inc(Result,sz);
  end;
end;

procedure Add_Ref(var Refs: TDualList; Ptr: Pointer);
var pi: PInteger;
    idx: integer;
begin
  if Refs=nil then Refs:=TDualList.Create;
  pi:=Ptr;
  idx:=Refs.IndexOf(pi);
  if idx>=0 then begin
    //if Refs.iv[idx]<>pi^ then
    Refs.iv[idx]:=pi^;
    exit;
  end;
  Refs.AddPiValue(pi,pi^);
end;

{$IFDEF WIN32} //@002+
procedure Clear_Refs(var Refs: TDualList);
var i: integer;
    pi: PInteger;
begin
  if Refs<>nil then begin
    for i:=0 to Refs.Count-1 do begin
      pi:=Refs[i];
      if not IsBadWritePtr(pi,4) and (pi^=integer(Refs.iv[i])) then
        pi^:=0;
    end;
    SaveFree(@Refs);
  end;
end;
{$ENDIF}//@002+

function CreateSortedList(Duplicates: TDuplicates): TStringList;
begin
  Result:=TStringList.Create;
  Result.Sorted:=True;
  Result.Duplicates:=Duplicates;
end;

{ TReplaceStream }

constructor TReplaceStream.Create(const FileName: string);
var Ext: string;
begin
  OriFile:=FileName;
  if FileExists(FileName) then begin
    FHaveOriginal:=True;
    Ext:=ExtractFileExt(FileName);
    Insert('~',Ext,2);
    TempFile:=ChangeFileExt(FileName,Ext);
  end else begin
    FHaveOriginal:=False;
    TempFile:=OriFile;
  end;
  Inherited Create(TempFile,fmCreate);
end;

constructor TReplaceStream.CreateCopy(const FileName: string; Dummy: integer=0);
var Src: TStream;
begin
  Create(FileName);
  if FHaveOriginal then begin
    // Copy current contents into new stream:
    Src:=TFileStream.Create(FileName,fmOpenRead or fmShareDenyWrite);
    try
      CopyStream(Src,Self,False);
    finally Src.Free; end;
    Seek(0,soFromBeginning);
  end;
end;

procedure TReplaceStream.Commit;
begin
  FCommited:=True;
end;

resourcestring
  SFileIsReadOnly='File %s is read-only!';
  SCannotRename='Cannot rename %s to %s!';

destructor TReplaceStream.Destroy;
begin
  try
    inherited;
  finally
    if not FCommited then
      DeleteFile(TempFile)
    else begin
      if FHaveOriginal then begin
        if not DeleteFile(OriFile) then
          raise Exception.CreateFmt(SFileIsReadOnly,[OriFile]);
        if not RenameFile(TempFile,OriFile) then
          raise Exception.CreateFmt(SCannotRename,[TempFile,OriFile]);
      end;//else already stored...
    end;
  end;
end;


// StackAlloc and StackFree borrowed from Borland's Grids.pas ...

{ StackAlloc allocates a 'small' block of memory from the stack by
  decrementing SP.  This provides the allocation speed of a local variable,
  but the runtime size flexibility of heap allocated memory.  }
function StackAlloc(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  CMP   EAX, 4092
  JLE   @@2
@@1:
  SUB   ESP, 4092
  PUSH  EAX          { make sure we touch guard page, to grow stack }
  SUB   EAX, 4096
  JNS   @@1
  ADD   EAX, 4096
@@2:
  SUB   ESP, EAX
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

function StackAllocClear(Size: Integer): Pointer; register;
asm
  POP   ECX          { return address }
  MOV   EDX, ESP
  ADD   EAX, 3
  AND   EAX, not 3   // round up to keep ESP dword aligned
  test  eax,eax
  jz    @@2
  shr   eax,2
@@1:
  push  0
  dec   eax
  jns   @@1
@@2:
  MOV   EAX, ESP     { function result = low memory address of block }
  PUSH  EDX          { save original SP, for cleanup }
  MOV   EDX, ESP
  SUB   EDX, 4
  PUSH  EDX          { save current SP, for sanity check  (sp = [sp]) }
  PUSH  ECX          { return to caller }
end;

{ StackFree pops the memory allocated by StackAlloc off the stack.
- Calling StackFree is optional - SP will be restored when the calling routine
  exits, but it's a good idea to free the stack allocated memory ASAP anyway.
- StackFree must be called in the same stack context as StackAlloc - not in
  a subroutine or finally block.
- Multiple StackFree calls must occur in reverse order of their corresponding
  StackAlloc calls.
- Built-in sanity checks guarantee that an improper call to StackFree will not
  corrupt the stack. Worst case is that the stack block is not released until
  the calling routine exits. }
procedure StackFree(P: Pointer); register;
asm
  POP   ECX                     { return address }
  MOV   EDX, DWORD PTR [ESP]
  SUB   EAX, 8
  CMP   EDX, ESP                { sanity check #1 (SP = [SP]) }
  JNE   @@1
  CMP   EDX, EAX                { sanity check #2 (P = this stack block) }
  JNE   @@1
  MOV   ESP, DWORD PTR [ESP+4]  { restore previous SP  }
@@1:
  PUSH  ECX                     { return to caller }
end;

{ TCompIntfList }

type
  // We need _Addref and _Release work...
  // We also need a persistent location, which can be AddReference'd by the object, so it can
  // clear itself, when it is destroyed...
  TCilItem=record
    Comp:          ICompUnknown;
    CrProc:        TCompUnknownCreateProc;
  end;
  PCilItem=^TCilItem;

// GuidToString and StringToGuid convert 16byte guid into and from 16char string (binary...)

function GuidToString(const IID: TGUID): string;
begin
  SetLength(Result,16);
  Move(IID,Result[1],16);
end;

function StringToGuid(const S: string; var IID: TGUID): Boolean;
begin
  if Length(S)<16 then
    Result:=False
  else begin
    Move(Pointer(S)^,IID,16);
    Result:=True;
  end;
end;

{$IFDEF WIN32}
constructor TCompIntfList.Create(AOwner: TComponent);
begin
  inherited;
  InitializeCriticalSection(FLock);
end;

destructor TCompIntfList.Destroy;
begin
  try
    Clear_Refs(Refs);
  except ; end;
  try
    Clear;
  except ; end;
  DeleteCriticalSection(FLock);
  inherited;
end;

procedure TCompIntfList.AddReference(PIntf: Pointer);
begin
  Add_Ref(Refs,PIntf);
end;
{$ENDIF}

{$IFDEF WIN32}
function TCompIntfList.QueryInterface(const IID: TGUID; out Obj): HResult;
begin
  //??? if someone adds interface to this list into the list, Search results in infinite recursion!
  if GetInterface(IID,Obj) or
     GetIntf(IID,Obj) or
     Search(IID,Obj,True)
  then Result:=S_OK
  else Result:=E_NOINTERFACE;
end;
{$ENDIF}

{$IFDEF WIN32}
procedure TCompIntfList.Lock;
begin
  EnterCriticalSection(FLock);
end;

procedure TCompIntfList.Unlock;
begin
  LeaveCriticalSection(FLock);
end;
{$ENDIF}

{$IFDEF WIN32}
procedure TCompIntfList.Clear;
var i: integer;
    pcil: PCilItem;
begin
  Lock;
  try
    if FItems=nil then exit;
    for i:=FItems.Count-1 downto 0 do begin
      pcil:=Pointer(FItems.Objects[i]);
      FItems.Delete(i);
      pcil.Comp:=nil;
      FreeMem(pcil);
    end;
    FreeAndNil(FItems);
  finally Unlock; end;
end;
{$ENDIF}

{$IFDEF WIN32}
procedure TCompIntfList.Pack;
var i: integer;
    pcil: PCilItem;
begin
  Lock;
  try
    if FItems=nil then exit;
    for i:=FItems.Count-1 downto 0 do begin
      pcil:=Pointer(FItems.Objects[i]);
      if (pcil.Comp=nil) and not Assigned(pcil.CrProc) then begin
        FItems.Delete(i);
        FreeMem(pcil);
      end;
    end;
    if FItems.Count=0 then
      FreeAndNil(FItems);
  finally Unlock; end;
end;

function TCompIntfList.GetCount: integer;
begin
  if FItems<>nil then Pack;
  if FItems=nil then begin
    Result:=0;
    exit;
  end;
  Result:=FItems.Count;
end;
{$ENDIF}

{$IFDEF WIN32}
function TCompIntfList.Get(Index: integer): ICompUnknown;
var pcil: PCilItem;
    IID: TGUID;
begin
  //if FItems<>nil then Pack;
  if (FItems=nil) or (Index<0) or (Index>=FItems.Count) then begin
    Result:=nil;
    exit;
  end;
  pcil:=Pointer(FItems.Objects[Index]);
  Result:=pcil.Comp;
  if (Result=nil) and Assigned(pcil.CrProc) then begin
    if StringToGuid(FItems[Index],IID)
       and not CreateItem(pcil,IID,Result)
    then
      Result:=nil;
  end;
end;

function TCompIntfList.GetIntf(const IID: TGUID; out Obj): Boolean;
var idx: integer;
    pcil: PCilItem;
    Name: string;
begin
  integer(Obj):=0; // Out param should not need _Release !
  Result:=False;
  if FItems=nil then
    exit;
  Lock;
  try
    Pack;
    if FItems<>nil then begin
      Name:=GuidToString(IID);
      idx:=FItems.IndexOf(Name);
      if idx>=0 then begin
        pcil:=PCilItem(FItems.Objects[idx]);
        if (pcil.Comp=nil) and Assigned(pcil.CrProc) then begin
          if not CreateItem(pcil,IID,Obj) then
            IUnknown(Obj):=nil;
        end;
        if (integer(Obj)=0) and (pcil.Comp<>nil) then
          Result:=(pcil.Comp.QueryInterface(IID,Obj)=0);
      end
    end;
  finally Unlock; end;
end;
{$ENDIF}

{$IFDEF WIN32}
function TCompIntfList.GetIID(Index: integer; var IID: TGUID): Boolean;
var gstr: string;
begin
  if (FItems=nil) or (Index<0) or (Index>=FItems.Count) then
    Result:=False
  else begin
    gstr:=FItems[Index];
    if gstr='' then
      Result:=False
    else begin
      Result:=StringToGuid(gstr,IID);
    end;
  end;
end;
{$ENDIF}

{$IFDEF WIN32}
function TCompIntfList.Search(const IID: TGUID; out Obj; AnonymousOnly: Boolean): Boolean;
var i: integer;
    pcil: PCilItem;
    bnPack: Boolean;
    IIDName: string;
begin
  integer(Obj):=0; // Out param should not need _Release !
  Result:=False;
  Lock;
  try
    bnPack:=False;
    if FItems<>nil then
      for i:=0 to FItems.Count-1 do begin
        if AnonymousOnly and (FItems[i]<>{?! was "="}'') then
          continue;
        pcil:=Pointer(FItems.Objects[i]);
        if pcil.Comp=nil then begin
          // Item was destroyed, or is dynamic:
          if not AnonymousOnly and Assigned(pcil.CrProc) then begin
            // Is dynamic... Would create it, only if registered with the searched guid:
            if IIDName='' then IIDName:=GuidToString(IID);
            if (IIDName=FItems[i]) then begin
              // Same registration:
              if not CreateItem(pcil,IID,Obj) then
                IUnknown(pcil.Comp):=nil;
              // Item would be queried again for iid...
            end;
          end;
          if pcil.Comp=nil then begin
            bnPack:=True;
            continue;
          end;
        end;
        try
          Result:=(pcil.Comp.QueryInterface(IID,Obj)=0);
        except ; end;
        if Result then
          break;
      end;
    try
      if not Result and (integer(Obj)<>0) then
        ICompUnknown(Obj):=nil;
    except ; end;
    if bnPack then Pack;
  finally Unlock; end;
end;

function TCompIntfList.AddNamed(const Name: string; const Obj: ICompUnknown): integer;
var pcil: PCilItem;
begin
  if not Assigned(Obj) then begin
    Result:=-1;
    exit;
  end;
  Lock;
  try
    if FItems=nil then begin
      FItems:=TStringList.Create;
      //! Not sorted! Must not use AnsiCompareText! Strings[?] may contain #0!
    end;
    pcil:=AllocMem(SizeOf(TCilItem));
    Result:=FItems.AddObject(Name,TObject(pcil));
    pcil.Comp:=Obj;
    Obj.AddReference(@pcil.Comp);
  finally Unlock; end;
end;

function TCompIntfList.Add(const IID: TGUID; const Obj: ICompUnknown): integer;
begin
  Result:=AddNamed(GuidToString(IID),Obj);
end;

function TCompIntfList.Add(const Obj: ICompUnknown): integer;
begin
  Result:=AddNamed('',Obj);
end;

function TCompIntfList.AddDynamic(const IID: TGUID; CreateProc: TCompUnknownCreateProc): integer;
var pcil: PCilItem;
begin
  Lock;
  try
    if FItems=nil then begin
      FItems:=TStringList.Create;
    end;
    pcil:=AllocMem(SizeOf(TCilItem));
    Result:=FItems.AddObject(GuidToString(IID),TObject(pcil));
    pcil.CrProc:=CreateProc;
  finally Unlock; end;
end;

function TCompIntfList.IsDynamic(Index: integer; var CreateProc: TCompUnknownCreateProc): Boolean;
var pcil: PCilItem;
begin
  Lock;
  try
    pcil:=nil;
    if (Index>=0) and (FItems<>nil) and (Index<FItems.Count) then
      pcil:=PCilItem(FItems.Objects[Index]);
    Result:=(pcil<>nil) and (pcil.Comp=nil) and Assigned(pcil.CrProc);
    if Result then
      CreateProc:=pcil.CrProc;
  finally Unlock; end;
end;

function TCompIntfList.CreateItem(APcil: Pointer; const IID: TGUID; out Obj): Boolean;
var pcil: PCilItem;
begin
  Result:=False;
  pcil:=APcil;
  if (pcil.Comp=nil) and Assigned(pcil.CrProc) and
     pcil.CrProc(Self,IID,pcil.Comp)
  then begin
    pcil.Comp.AddReference(@pcil.Comp);
    ICompUnknown(Obj):=pcil.Comp;
    pcil.Comp._Release; // Only the caller would hold a reference & object will auto-clear itself from the list, when destroyed (if it is ref-counted)...
    Result:=True;
  end;
end;

function TCompIntfList.Merge(Source: ICompIntfList): Boolean;
var i: integer;
    IID: TGuid;
    Comp: ICompUnknown;
    CrProc: TCompUnknownCreateProc;
begin
  Result:=False;
  if Source=nil then exit;
  Source.Lock;
  try
    for i:=0 to Source.Count-1 do begin
      if Source.IsDynamic(i,CrProc) then begin
        if Source.GetIID(i,IID) then
          AddDynamic(IID,CrProc);
        continue;
      end;
      Comp:=Source.Get(i);
      if Comp=nil then continue;
      Result:=True;
      if Source.GetIID(i,IID) then
        Add(IID,Comp)
      else
        Add(Comp);
    end;
  finally Source.Unlock; end;
end;

function TCompIntfList.IndexOfObject(const Obj: ICompUnknown): Integer;
var pcil: PCilItem;
    i: integer;
begin
  if FItems<>nil then Pack;
  for i:=FItems.Count-1 downto 0 do begin
    pcil:=Pointer(FItems.Objects[i]);
    if pcil.Comp=Obj then begin
      Result:=i;
      exit;
    end;
  end;
  Result:=-1;
end;

function TCompIntfList.IndexOf(const IID: TGUID): integer;
begin
  if FItems<>nil then Pack;
  if FItems=nil then
    Result:=-1
  else
    Result:=FItems.IndexOf(GuidToString(IID));
end;

function TCompIntfList.Remove(const Obj: ICompUnknown): integer;
var pcil: PCilItem;
begin
  Lock;
  try
    Result:=IndexOfObject(Obj);
    if Result>=0 then begin
      pcil:=Pointer(FItems.Objects[Result]);
      FItems.Delete(Result);
      pcil.Comp:=nil;
      FreeMem(pcil);
      if (Result=0) and (FItems.Count=0) then
        FreeAndNil(FItems); 
    end;
  finally Unlock; end;
end;
{$ENDIF}

{$IFDEF WIN32}
var
  LocalCompList: TCompIntfList=nil;
  Local_Cil: ICompIntfList=nil;
{$ENDIF}

{$IFDEF WIN32}
function GetCompList: ICompIntfList; // Returns (AppMain as ICompIntfList) or creates private list, which can AppMain later merge...
begin
  Result:=nil;
  if (AppMain=nil)
     or (AppMain.QueryInterface(ICompIntfList,Result)<>0)
     or (Result=nil)
  then begin
    if not Assigned(Local_Cil) then begin
      if LocalCompList=nil then begin
        LocalCompList:=TCompIntfList.Create(nil);
        LocalCompList.AddReference(@LocalCompList);
      end;
      if LocalCompList.GetInterface(ICompIntfList,Local_Cil) then
        LocalCompList.AddReference(@Local_Cil);
    end;
    Result:=Local_Cil;
  end;
  if not Assigned(Result) then // Unprobable, but don't produce FFFFFFFF later... This function guarantess the result.
    Abort;
end;
{$ENDIF}

{$IFDEF WIN32}//@002+
procedure DropInternalCompList; // Only if (AppMain<>nil) and Supports(AppMain,ICompIntfList) !
var Main_Cil: ICompIntfList;
begin
  if Local_Cil=nil then exit;
  Main_Cil:=GetCompList;
  if Main_Cil<>Local_Cil then begin
    Local_Cil:=nil; // Done through AddReference watching...
    SaveFree(@LocalCompList);
  end;
end;
{$ENDIF}//@002+

initialization
finalization
 // if SubMasterLock_Init then
 //   DeleteCriticalSection(SubMasterLock);
  if DeleteList<>nil then begin
    DeleteFiles(DeleteList,False);
    {$IFDEF WIN32}//@002+
    SaveFree(@DeleteList);
    {$ELSE}          //@002+
    DeleteList.Free; //@002+
    {$ENDIF}         //@002+
  end;
end.

