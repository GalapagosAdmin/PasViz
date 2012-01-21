unit libPasViz;
// @001 2011.12.29 Noah Silva + Initial Creation of Unit
// @003 2011.12.30 Noah Silva = Changes to accomidate PasViz GUI
// @004 2011.12.31 Noah Silva = Changes for Windows
// @005 2012.01.04 Noah Silva = Misc. Clean-up
//                            = Eliminate double-lines in unit relationships
// @006 2012.01.05 Noah Silva = Debugging.
// @007 2012.01.09 Noah Silva = Minor Fixes (Force absolute path)
// @008 2012.01.11 Noah Silva = DebugLn, StringList fixes
{$mode objfpc}{$H+}

{$DEFINE DEBUG}

interface

uses
  Classes, SysUtils;

ResourceString
  msgDepthExceeded = 'Maximum depth exceeded, exiting.';
  msgUnitNotFound = 'Error: Unit file %s could not be located.';
  msgProjectNotFound = 'Error: Project file %s could not be located.';
  msgProcessing = 'Processing %s...';
  msgProcessingDone = 'Processing Complete.';                                   //@005+

Const
  StyleUnitNotFound = ' [color=red]';
  StyleImplementation = 'edge [style=solid]';
  StyleInterface = 'edge [style=dotted]';
  StyleMain = ' [shape=box]';
  StyleNormalUnit = 'node [shape=ellipse]';

// Process and entire project, given the main program filename
 Procedure ProcessProject(Const ProjectName:UTF8String);

// Accepts a unit name with no file extension
// Returns true if the unit exists with .pas or .pp extension
 Function UnitExists(Const UnitName:UTF8String):Boolean;
 Function GetUnitFileName(Const UnitName:UTF8String):UTF8String;
 Function UnitSeenCheck(Const UnitName:UTF8String):Boolean;
 Function UnitSeenMark(Const UnitName:UTF8String):Boolean;
 Function Get_Output_Filename(Const ProjectFileName:UTF8String):UTF8String;     //@003=
 Function Get_PNG_Filename(Const ProjectFileName:UTF8String):UTF8String;        //@004+
 Function GraphViz_exe_get:UTF8String;                                          //@008+
 procedure GraphViz_exe_set(const exepath:UTF8String);                          //@008+


implementation

Uses
  FileUtil, DbugIntf, PasParser, libDot,
  xmlconf;                                                                      //@008+

var
  SeenUnits:TStringList;
  SeenRelations:TStringList;                                                    //@005+
  Steps:Integer;   // calls to process_Unit                                     //@008+
  XMLConfig:TXMLConfig;                                                         //@008+

Procedure DebugLn(Const Data:UTF8String);                                       //@008+
  begin
    {$IFDEF WIN32}
    // Writeln will crash under windows if not running in console mode unless
    // compiled as non-gui application.  If you turn off GUI mode, then the
    // console will always remain.
 //   Writeln(Data);

    SendDebug(Data);
    {$ELSE}
    Writeln(Data);
    {$ENDIF}
  end;

Function UnitExists(Const UnitName:UTF8String):Boolean;
  begin
    Result := FileExistsUTF8(UnitName+'.pas');
    if result then DebugLn(unitname+'.pas found.');
    If not Result then
      begin
      Result := FileExistsUTF8(UnitName+'.pp');
        if result then DebugLn(unitname+'.pp found.');
    end;
    if not result then DebugLn(unitname + ' not found');
  end;

Function GetUnitFileName(Const UnitName:UTF8String):UTF8String;
  begin
    // First, check current directory
    if FileExistsUTF8(UnitName+'.pas') then  // with .pas
      Result := UnitName + '.pas'
    else if FileExistsUTF8(UnitName+'.pp') then  // and with .pp
      Result := UnitName+'.pp'
      // then we should check the lpi file for other directories to check
      // and then FPC/Lazarus configuration
    else  // if all fails, we give a blank result to indicate "we don't know".
      Result := '';
  end;

Function UnitSeenCheck(Const UnitName:UTF8String):Boolean;
 var
   idx:Integer;
 begin
   Result := SeenUnits.Find(LowerCase(UnitName), idx);                          //@005=
   // or use .IndexOf for non-sorted Stringlists
 end;

Function UnitSeenMark(Const UnitName:UTF8String):Boolean;
  begin
    Result := UnitSeenCheck(UnitName);
    If not Result then SeenUnits.Add(LowerCase(UnitName));                      //@005=
  end;

// Returns true is a relationship between these two units (in the specified
// order) has been seen before.
Function RelationSeenCheck(Const Unit1, Unit2:UTF8String):Boolean;              //@005+
 var
   idx:Integer;
 begin
   Result := SeenRelations.Find(LowerCase(Unit1+':'+Unit2), idx);
      // or use .IndexOf for non-sorted Stringlists
 end;

// Returns true is a relationship between these two units (in the specified
// order) has been seen before, and marks the relationship as seen, if it was
// not already.
Function RelationSeenMark(Const Unit1, Unit2:UTF8String):Boolean;               //@005+
  begin
    Result := RelationSeenCheck(Unit1, Unit2);
    If not Result then SeenRelations.Add(Lowercase(Unit1+':'+Unit2));
  end;


Procedure ProcessUnit(Const UnitName:UTF8String; Depth:Integer; DF:TDotFile);   //@003=
// UnitName = File name of unit (PAS) file, without extension
  Const
    maxDepth=10;
  var
    s:ShortString;
    i:Integer;
    _Unit:TUnit;
    UnitNo:Word;
  begin

//    s := '';
//    for i := 1 to depth do s := s + '>';
    Inc(Steps);                                                                 //@008+
    DebugLn(Inttostr(steps) + '  ' + inttostr(depth) + ' ' + UnitName);

    // Skip processing this unit if it has already been processed.
    If UnitSeenMark(UnitName) then
      begin
        DebugLn(UnitName + ' already seen, exiting');
        exit;
      end
    else
       DebugLn(UnitName + ' is new, Processing...');
    // Sanity check in case of some kind of dependency loop.
    if depth >= MaxDepth then
      begin
         DebugLn(msgDepthExceeded);                                             //@003=
         exit;
      end;
    // If we can't find this unit, then we just output a node for it, with no
    // sub-nodes (since we can't search the file for used units).
    If not UnitExists(UnitName) then                                            //@001=
      begin
        DebugLn(Format(msgUnitNotFound, [UnitName]));                           //@003=
        DF.DotOut(LowerCase(UnitName) + StyleUnitNotFound);                     //@001=//@003=
        exit;
      end;
    DF.DotOut(StyleNormalUnit); // project gets a box
    SendDebug(Format(msgProcessing, [UnitName]));                               //@001=//@003=
    try
      _Unit:=TUnit.Create(nil, GetUnitFileName(UnitName));                      //@006=
      _Unit.Parse;
      // We don't output our own node, since that was done at the level above.
      // If there are no nodes below us, nothing left to do here.
      If _Unit.UsedUnits.Count = 0 then exit;
      // Loop through the used units
      for UnitNo := 0 to _Unit.UsedUnits.Count-1 do
        begin
           // Skip relations that have already been processed                   //@005+
           If RelationSeenMark(_Unit.UnitName,                                  //@005+
                                _Unit.UsedUnits[UnitNo]) then                   //@005+
             begin
               DebugLn('Relation already seen:' + _Unit.UnitName                //@007+
                       + ':' + _Unit.UsedUnits[UnitNo]);                        //@007+
              continue;                                                         //@005+
             end;
           // Check the opposite direction too...
//           If RelationSeenMark(_Unit.UsedUnits[UnitNo], _Unit.UnitName        //@005+-
//                                ) then                                        //@005+-
//              continue;                                                       //@005+-
        // Output Unit Style, depending upon which USES section it's referenced
        // from. (interface vs implementation section)
         IF _Unit.UsedUnits.Objects[UnitNo] = BoolObjFalse then                 //@002+
            // Implementation
            DF.DotOut(StyleImplementation)                                      //@003=
         else // Interface
            DF.DotOut(StyleInterface);                                          //@003=
         // Output Unit relationship (Arrow)
         DF.DotOut(LowerCase(_Unit.UnitName) + '->'
           + LowerCase(_Unit.UsedUnits[UnitNo]));                               //@003=
         // Then process this unit (and its dependencies).
//         If not UnitSeenMark(_Unit.UsedUnits[UnitNo]) then                      //@005+//@007- Double Jeopardy
           ProcessUnit(_Unit.UsedUnits[UnitNo], depth+1, DF);                   //@003=
        end; // of UnitNo loop;
    finally
      _Unit.free;
    end;


  end;

Function Get_Output_Filename(Const ProjectFileName:UTF8String):UTF8String;      //@003=
  begin
    Result := ExpandFileName(ChangeFileExt(ProjectFileName, '.gv'));            //@007=
  end;

Function Get_PNG_Filename(Const ProjectFileName:UTF8String):UTF8String;         //@004+
  begin
    Result := ExpandFileName(ChangeFileExt(ProjectFileName, '.png'));           //@007=
  end;

// Re-Init for re-use
Procedure re_init;                                                              //@008+
  begin
    SeenUnits.Clear;
    SeenRelations.Clear;
  end;

Procedure ProcessProject(Const ProjectName:UTF8String);
// ProjectName = File name of project (LPR) file, including extension
  var
    _Project:TProject;
    UnitNo:Word;
    DF:TDotFile;
    Orig_Dir:UTF8String;                                                        //@007+
  begin
    re_init;                                                                    //@008+
    Orig_Dir := GetCurrentDirUTF8;                                              //@007+
    If not FileExistsUTF8(ProjectName) then
      begin
        DebugLn(Format(MsgProjectNotFound, [ProjectName]));                     //@001=//@003=
        exit;
      end;
    DebugLn(Format(msgProcessing, [ProjectName]));                              //@001=//@003=
    try
      DF := TDotFile.Create(Get_Output_Filename(ProjectName));                  //@003=
      _Project:=TProject.Create(nil, ProjectName);
      _Project.Parse;
      DF.DotOut(Lowercase(_Project.UnitName) + StyleMain); // project gets a box
      If _Project.UsedUnits.Count = 0 then exit;
      for UnitNo := 0 to _project.UsedUnits.Count-1 do
        begin
         // Skip relationship if already seen.
         If RelationSeenMark(_Project.UnitName,                                 //@005+
                              _Project.UsedUnits[UnitNo]) then                  //@005+
            continue;                                                           //@005+
         // this should never happen, since main programs don't have
           // an interface section.
         IF _Project.UsedUnits.Objects[UnitNo] = BoolObjFalse then              //@002=
           DF.DotOut(LowerCase(_Project.UnitName) + '->'                        //@003=
             + LowerCase(_Project.UsedUnits[UnitNo]))
         else    // this is a normal uses from a main program
           DF.DotOut(LowerCase(_Project.UnitName) + '->'                        //@003=
             + LowerCase(_Project.UsedUnits[UnitNo]));
         // Process this unit and any dependencies.
         ProcessUnit(lowercase(_Project.UsedUnits[UnitNo]), 0, DF);             //@001=//@003=
        end; // of UnitNo loop;
      SendDebug(msgProcessingDone);                                             //@005=
      // The following shouldn't be necessary, but the destructor is not
      // properly closing the file on Win32.
      DF.FileClose;                                                             //@004+
    finally
      _Project.free;
      DF.Free;
      SetCurrentDirUTF8(Orig_Dir);                                              //@007+
    end;
  end;

procedure GraphViz_exe_set(const exepath:UTF8String);                           //@008+
  const
    {$IFDEF WIN32}
    ConfigKey='gvexe_win32';
    {$ENDIF}
    {$IFDEF DARWIN}
    ConfigKey= 'gvexe_darwin';
    {$ENDIF}
  begin
    XMLConfig.SetValue(ConfigKey, exepath)
  end;

Function GraphViz_exe_get:UTF8String;
  begin
  {$IFDEF WIN32}
  Result:=XMLConfig.GetValue('gvexe_win32', 'c:\Program Files\Graphviz 2.28\bin\dot.exe');
  {$ENDIF}
  {$IFDEF UNIX}
    {$IFDEF DARWIN}
    Result := XMLConfig.GetValue('gvexe_darwin', '/Applications/Graphviz.app/Contents/MacOS/Graphviz');
    {$ELSE} // Linux or BSD?
    Result := '/usr/bin/dot';
    {$ENDIF}
  {$ENDIF}
  end;

//http://wiki.lazarus.freepascal.org/Executing_External_Programs
//function GetTempDir: String;

procedure do_init;
  begin
    try
    XMLConfig := TXMLConfig.Create(nil);                                        //@008+
    ForceDirectories(GetAppConfigDir(False));                                   //@008+
    XMLConfig.Filename:=                                                        //@008+
                                                     GetAppConfigFile(False);   //@008+
  //  Writeln(XMLConfig.Filename);

    finally
      SeenUnits := TStringList.Create;
      SeenUnits.Sorted := True;                                                   //@008+
      SeenRelations := TStringList.Create;                                        //@005+
      SeenRelations.Sorted := True;                                               //@008+
      Steps := 0;                                                                 //@008+
    end;
  end;

initialization
  do_init;

finalization
 // DebugLn('libPasViz.Finalization Begin');
  SeenUnits.Free;
  SeenRelations.Free;
  XMLConfig.SetValue('appname', 'pasviz');//@005+
  XMLConfig.Flush;                                                              //@008+
  XMLConfig.Free;                                                               //@008+
//  DebugLn('libPasViz.Finalization End');

end.

