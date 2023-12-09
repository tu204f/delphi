program bybit;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.SysUtils in '..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Candels in '..\lb\Lb.Bybit.Candels.pas',
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Bybit.Announcement in '..\lb\Lb.Bybit.Announcement.pas',
  Lb.Bybit.InstrumentsInfo in '..\lb\market\Lb.Bybit.InstrumentsInfo.pas',
  Lb.Bybit.ServerTime in '..\lb\market\Lb.Bybit.ServerTime.pas',
  Lb.Bybit.Kline in '..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bybit.Encryption in '..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.Position in '..\lb\position\Lb.Bybit.Position.pas',
  Lb.Bybit.PlaceOrder in '..\lb\trade\Lb.Bybit.PlaceOrder.pas',
  Lb.Bybit in '..\lb\Lb.Bybit.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
