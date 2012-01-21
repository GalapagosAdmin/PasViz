unit frmoutputunit;
//@001 2012.01.11 Noah Silva : Added Zoom Functions
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, ExtCtrls,
  ComCtrls, ActnList;//, frmMainUnit;

type

  { TfrmOutput }

  TfrmOutput = class(TForm)
    acSaveImage: TAction;
    acZoomActual: TAction;
    acZoomOut: TAction;
    acZoomIn: TAction;
    alOutput: TActionList;
    ilOutput: TImageList;
    imgOutput: TImage;
    ScrollBox1: TScrollBox;
    ToolBar1: TToolBar;
    tbtnZoomIn: TToolButton;
    tbtnZoomOut: TToolButton;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    procedure acSaveImageExecute(Sender: TObject);
    procedure acZoomActualExecute(Sender: TObject);
    procedure acZoomInExecute(Sender: TObject);
    procedure acZoomOutExecute(Sender: TObject);
    procedure ToolBar1Click(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmOutput: TfrmOutput;

implementation
  uses math;//, libPasViz;

{$R *.lfm}

{ TfrmOutput }

procedure TfrmOutput.acZoomInExecute(Sender: TObject);                          //@001+
begin
  imgOutput.AutoSize:=False;
  imgOutput.Stretch:=True;
  // Align has to be set to "none" for this to work.
  imgOutput.Width:=floor(imgOutput.Width*1.25);
  imgOutput.Height:=floor(imgOutput.Height*1.25);
end;

procedure TfrmOutput.acZoomActualExecute(Sender: TObject);                      //@001+
begin
  imgOutput.AutoSize:=True;
  imgOutput.Stretch:=False;
  imgOutput.Refresh;
end;

procedure TfrmOutput.acSaveImageExecute(Sender: TObject);
begin
  With TSaveDialog.Create(Self) do
  try
    DefaultExt:='.png';
    Filter:= 'PNG Files|*.png';
    If Execute then
      imgOutput.Picture.SaveToFile(FileName);
  finally
    free;
  end;
end;

procedure TfrmOutput.acZoomOutExecute(Sender: TObject);                         //@001+
begin
  imgOutput.AutoSize:=False;
  imgOutput.Stretch:=True;
  imgOutput.Width:=floor(imgOutput.Width/1.25);
  imgOutput.Height:=floor(imgOutput.Height/1.25);
  imgOutput.Refresh;
end;

procedure TfrmOutput.ToolBar1Click(Sender: TObject);
begin

end;

end.

