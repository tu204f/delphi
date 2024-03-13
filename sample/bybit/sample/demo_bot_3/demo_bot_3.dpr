program demo_bot_3;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Indicator in '..\..\lb\indicator\Lb.Indicator.pas',
  Lb.Mode in '..\..\..\bot_14122023\lb\Lb.Mode.pas',
  Lb.Position.Trade in '..\..\..\bot_14122023\lb\Lb.Position.Trade.pas',
  Lb.SysUtils in '..\..\..\bot_14122023\lb\Lb.SysUtils.pas',
  UnitBotFrame in 'UnitBotFrame.pas' {BotFrame: TFrame},
  UnitIndicatorFrame in 'UnitIndicatorFrame.pas' {IndicatorFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
