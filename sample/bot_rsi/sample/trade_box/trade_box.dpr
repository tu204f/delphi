program trade_box;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.TradeBox in '..\..\bq_rsi_2\lb\Lb.TradeBox.pas',
  Lb.SysUtils in '..\..\bq_rsi_2\lb\Lb.SysUtils.pas',
  Lb.Level in '..\..\bq_rsi_2\lb\Lb.Level.pas',
  Lb.Journal.Trading in '..\..\bq_rsi_2\lb\Lb.Journal.Trading.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
