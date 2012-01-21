unit frmConfigUnit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  EditBtn;

type

  { TfrmConfig }

  TfrmConfig = class(TForm)
    ebConfig_gvexe: TFileNameEdit;
    Label1: TLabel;
    procedure ebConfig_gvexeChange(Sender: TObject);
    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmConfig: TfrmConfig;

implementation
  uses
    libPasViz;

{$R *.lfm}

{ TfrmConfig }

procedure TfrmConfig.FormShow(Sender: TObject);
begin
  ebConfig_gvexe.Text:=GraphViz_exe_get;
end;

procedure TfrmConfig.ebConfig_gvexeChange(Sender: TObject);
begin
  GraphViz_exe_set(ebConfig_gvexe.Text);
end;

end.

