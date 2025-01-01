program vt;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Journal.Trading.V2 in '..\..\bq_rsi_2\lb\Lb.Journal.Trading.V2.pas',
  Lb.SysUtils in '..\..\bq_rsi_2\lb\Lb.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
