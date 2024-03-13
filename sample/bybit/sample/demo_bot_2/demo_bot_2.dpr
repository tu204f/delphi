program demo_bot_2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Mode in '..\..\..\bot_14122023\lb\Lb.Mode.pas',
  Lb.SysUtils in '..\..\..\bot_14122023\lb\Lb.SysUtils.pas',
  Lb.Position.Trade in '..\..\..\bot_14122023\lb\Lb.Position.Trade.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
