unit FrmMainUnit;
//@000 2011.12.30 Noah Silva : Started PasVizGUI Project
//@006 2012.01.11 Noah Silva : Various Fixes on Win32
//                             Improvements to internal viewer
{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, Forms, Controls, Graphics, Dialogs, StdCtrls,
  Buttons, EditBtn{, UTF8Process}, ActnList, process;

type

  { TfrmMain }

  TfrmMain = class(TForm)
    acnProcessProject: TAction;
    acnCheckFile: TAction;
    acnInit: TAction;
    ActionList1: TActionList;
    bbProcess: TBitBtn;
    ebProjectFile: TFileNameEdit;
    lblProjectFile: TLabel;
    Process1: TProcess;
    sbConfig: TSpeedButton;
    procedure acnCheckFileExecute(Sender: TObject);
    procedure acnInitExecute(Sender: TObject);
    procedure acnProcessProjectExecute(Sender: TObject);
//    procedure bbProcessClick(Sender: TObject);
    procedure ebProjectFileAcceptFileName(Sender: TObject; var Value: UTF8String);
    procedure ebProjectFileChange(Sender: TObject);
    procedure sbConfigClick(Sender: TObject);
//    procedure FormShow(Sender: TObject);
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  frmMain: TfrmMain;

  {$R *.lfm}

implementation

Uses libPasViz
      {$IFNDEF DARWIN}
     ,  frmoutputunit
      {$ENDIF}
      , frmconfigunit
      ;//@004+

ResourceString
  msgFileNotExist = 'Unable to locate project file.';
  MsgExeNotFound = 'It appears GraphViz is not installed.  Please download and install it before using PasViz';

{ TfrmMain }


procedure TfrmMain.ebProjectFileAcceptFileName(Sender: TObject;
  var Value: UTF8String);
begin
 // acnCheckFile.Execute;
  bbProcess.enabled := true;
end;

procedure TfrmMain.ebProjectFileChange(Sender: TObject);
begin

end;

procedure TfrmMain.sbConfigClick(Sender: TObject);
begin
  frmConfig.show;
end;


procedure TfrmMain.acnProcessProjectExecute(Sender: TObject);
begin
  // check input project source file
  If not FileExistsUTF8(ebProjectFile.Text) then
     begin
       ShowMessage(msgFileNotExist);
       exit;
     end;
   ProcessProject(ebProjectFile.Text);
   // MacOS System Page is UTF8, no conversion required
   With Process1 do
     begin
       {$IFDEF DARWIN}
       // in Mac OS X, we just tell the system to open the dot (gv) file we made
       // the default Graphviz viewer will show it on the screen.
       CommandLine := '/usr/bin/open '+ Get_Output_Filename(ebProjectFile.Text);      //@003=
   //    Showmessage(CommandLine);
       {$ENDIF}
       {$IFDEF WIN32}
       // In Windows, we just run DOT to process the file into a PNG file.
       CommandLine := '"'+ GraphViz_exe_get +'" -Tpng '
                       + Get_Output_Filename(ebProjectFile.Text)
                       + ' -o '
                       + Get_PNG_Filename(ebProjectFile.Text)
                        ;
       //Writeln(CommandLine);

       {$ENDIF}

       Options := Options + [poWaitOnExit];
       execute;

       {$IFDEF WIN32}  // we use our custom output display if in windows
       {$IFDEF INTERNALVIEWER}
       If FileExistsUTF8(Get_PNG_Filename(ebProjectFile.Text)) then        //@006+
          with FrmOutputUnit.frmOutput do
          begin                                                                 //@006+
            with imgOutput.Picture do                                           //@006=
              LoadFromFile(Get_PNG_Filename(ebProjectFile.Text));
//            Caption:= ApplicationName + ' - '                                   //@006=
            Caption:= 'PasViz' + ' - '                                          //@006=
               + ExtractFileName(Get_PNG_Filename(ebProjectFile.Text));
            Show;
          end
          else
            Showmessage('Output PNG file could not be located.');
     end;
         {$ELSE} // or use START command to launch system default PNG viewer

          CommandLine := 'cmd /c start '
                       + Get_PNG_Filename(ebProjectFile.Text)
                        ;
          execute;
          {$ENDIF} //of if custom viewer
      {$ENDIF} // of IF windows
       // Linux should perhaps be handled the same way as Windows.
       // (Depends if there is a decent native viewer already or not).

     end;
end;

procedure TfrmMain.acnCheckFileExecute(Sender: TObject);
begin
    bbProcess.Enabled:= (Length(ebProjectFile.Text) > 0);
end;

procedure TfrmMain.acnInitExecute(Sender: TObject);


begin
//  set in GUI instead
  //ebProjectFile.Filter:='Lazarus Project|*.lpr|Delphi Project|*.dpr|Pascal Project|*.pas|All Files|*';
  {$IFDEF WIN32}
  If Not FileExistsUTF8(GraphViz_exe_get) Then                                  //@006=
     ShowMessage(MsgExeNotFound);
  {$ENDIF}

end;



end.

