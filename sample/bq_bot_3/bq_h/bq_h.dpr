program bq_h;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.SysUtils in 'lb\Lb.SysUtils.pas',
  Lb.Breakdown in 'lb\Lb.Breakdown.pas',
  Lb.Bot in 'lb\Lb.Bot.pas',
  Lb.Journal.Trading in 'lb\Lb.Journal.Trading.pas',
  UnitPositionFrame in 'lb\frame\UnitPositionFrame.pas' {PositionFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
