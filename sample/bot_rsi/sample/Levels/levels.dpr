program levels;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Level in '..\..\lib\Lb.Level.pas',
  UnitLevelsFrame in 'UnitLevelsFrame.pas' {LevelsFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
