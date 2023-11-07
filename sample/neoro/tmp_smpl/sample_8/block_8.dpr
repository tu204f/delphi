program block_8;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Block in 'Lb\Lb.Block.pas',
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.ActivationFunction in '..\lib\Lb.ActivationFunction.pas',
  Lb.NeuronNet in '..\lib\Lb.NeuronNet.pas',
  UnitChartCandelsFrame in 'Lb\UnitChartCandelsFrame.pas' {ChartCandelsFrame: TFrame},
  Lb.JournalTrades in 'Lb\Lb.JournalTrades.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  UnitCharTradeFrame in 'Lb\UnitCharTradeFrame.pas' {CharTradeFrame: TFrame},
  Lb.Sources in 'Lb\Lb.Sources.pas',
  UnitMainFrame in 'UnitMainFrame.pas' {MainFrame: TFrame},
  UnitLogFrame in 'UnitLogFrame.pas' {LogFrame: TFrame},
  Lb.SysUtils in 'Lb.SysUtils.pas',
  Lb.JournalTrades.V2 in 'Lb\Lb.JournalTrades.V2.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  TLogger.ClearLog;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
