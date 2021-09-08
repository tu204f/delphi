program create_db;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Create.DB in 'lib\Lb.Create.DB.pas',
  Lb.FieldFrame in 'lib\frame\Lb.FieldFrame.pas' {FieldFrame: TFrame},
  Lb.IndexFrame in 'lib\frame\index\Lb.IndexFrame.pas' {IndexFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
