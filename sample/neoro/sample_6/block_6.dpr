program block_6;

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
  Lb.Logger in '..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  TLogger.ClearLog;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
