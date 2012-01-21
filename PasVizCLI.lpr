program PasVizCLI;
// @000 2011.12.28 Noah Silva + Started
// @001 2011.12.19 Noah Silva + Moved Styles into Constants
// @003 2011.12.?? Noah SILVA + Debugging/Minor Fixes
// @004 2012.01.09 Noah Silva = Fixes
// To-Do:
// 1. Differentiate forms, data modules, and frames
// 2. Make an ignore list for system units
{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Classes, SysUtils, CustApp//, fileutil
  , libPasViz                                                                   //@002+
  , Process                                                                     //@003+
  { you can add units after this };

type

  { TPasVizCLI }

  TPasVizCLI = class(TCustomApplication)
  protected
    procedure DoRun; override;
  public
    constructor Create(TheOwner: TComponent); override;
    destructor Destroy; override;
    procedure WriteHelp; virtual;
  end;

{ TPasVizCLI }

procedure TPasVizCLI.DoRun;
var
  ErrorMsg: String;
begin
  // quick check parameters
  ErrorMsg:=CheckOptions('h','help');
  if ErrorMsg<>'' then begin
    ShowException(Exception.Create(ErrorMsg));
    Terminate;
    Exit;
  end;

  // parse parameters
  if HasOption('h','help') or (ParamCount = 0) then
    begin
     WriteHelp;
     Terminate;
     Exit;
    end;

  { add your program here }

  If ParamCount = 1 then
    begin
     ProcessProject(ParamStr(1));
     {$IFDEF DARWIN}
       With TProcess.Create(nil) do                                             //@003+
        begin                                                                   //@003+
          // The following logic can cause problems if the working directory
          // is not the main program directory
       CommandLine:='/usr/bin/open '+ Get_Output_Filename(ParamStr(1));         //@003+
       Writeln('Command Line:' + CommandLine);
       Options := Options + [poWaitOnExit];                                     //@003+
       execute;                                                                 //@003+
     end;                                                                       //@003+
    {$ENDIF}
    end;
  // stop program loop
  Terminate;
end;

constructor TPasVizCLI.Create(TheOwner: TComponent);
begin
  inherited Create(TheOwner);
  StopOnException:=True;
end;

destructor TPasVizCLI.Destroy;
begin
  inherited Destroy;
end;

procedure TPasVizCLI.WriteHelp;
begin
  { add your help code here }
  writeln('Usage: ',ExeName,' -h | ProjectFile.lpr');
end;

var
  Application: TPasVizCLI;
begin
  Application:=TPasVizCLI.Create(nil);
  Application.Run;
  Application.Free;
end.
