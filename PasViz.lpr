program PasViz;

{$mode objfpc}{$H+}

uses
  {$IFDEF UNIX}{$IFDEF UseCThreads}
  cthreads,
  {$ENDIF}{$ENDIF}
  Forms, Interfaces, FrmMainUnit, ComUtil, frmoutputunit, frmconfigunit
  { you can add units after this };

{$R *.res}

begin
 // RequireDerivedFormResource := True;  // no idea about this - used in Laz 0.9.31?
  Application.Initialize;
  Application.CreateForm(TfrmMain, frmMain);
  Application.CreateForm(TfrmOutput, frmOutput);
  Application.CreateForm(TfrmConfig, frmConfig);
  Application.Run;
end.

