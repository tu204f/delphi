program trader;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.PositionTrade in 'lb\Lb.PositionTrade.pas',
  UnitTraderFrame in 'trader\UnitTraderFrame.pas' {TraderFrame: TFrame},
  UnitChartDataFrame in 'trader\UnitChartDataFrame.pas' {ChartDataFrame: TFrame},
  UnitTraderFrame.V2 in 'trader\UnitTraderFrame.V2.pas' {Frame1: TFrame},
  Lb.ReadPrice in '..\lib\Lb.ReadPrice.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
