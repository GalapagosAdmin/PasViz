unit libDot;
//@000 2011.12.28 Noah Silva : Started Library
//@003 2011.12.30 Noah Silva = Converted to OOP
//@005 2012.01.04 Noah Silva = Minor Changes
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils; 

Type
  TDotFile=Class(TObject)                                                       //@003=
     private
        _dotfilename:UTF8String;
        _dotfile:System.text;
     public
      Procedure FileOpen;
      Procedure FileClose;
      Procedure DotOut(Const Txt:UTF8String);
      Constructor Create(Const OutputFilename:UTF8String);
      Destructor Destroy;
    end;

implementation



Procedure TDotFile.FileOpen;                                                    //@003=
  begin
     system.assign(_dotfile, _DotFileName);
     system.rewrite(_dotfile);
     // Strict Diagraph will tell GraphVis to supress multiple links between the
     // same nodes.  We should supress them in out code, though.
//     writeln(_dotfile, 'strict digraph output {');                            //@005-
     writeln(_dotfile, 'digraph output {');                                     //@005+
  end;

Procedure TDotFile.FileClose;                                                   //@003=
  Begin
    writeln(_dotfile, '}');
    system.close(_dotfile);
  end;

Procedure TDotFile.DotOut(Const Txt:UTF8String);
 begin
  // writeln(txt);
   writeln(_dotfile, txt);
 end;

Constructor TDotFile.Create(Const OutputFilename:UTF8String);                   //@003=
  begin
    _DotFileName := OutputFilename;
    FileOpen;
  end;

Destructor TDotFile.Destroy;                                                    //@003=
  begin
    try
      FileClose;
    finally
      inherited destroy;
    end;
  end;

initialization

finalization
 // Writeln('libDot.Finalization Begin');
 // Writeln('libDot.Finalization End');

end.
