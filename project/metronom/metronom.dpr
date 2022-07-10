program metronom;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  UnitNumberBitFrame in 'UnitNumberBitFrame.pas' {NumberBitFrame: TFrame},
  UnitBeatsPerMinuteFrame in 'UnitBeatsPerMinuteFrame.pas' {BeatsPerMinuteFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
