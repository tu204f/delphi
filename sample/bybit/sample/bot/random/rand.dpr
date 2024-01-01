program rand;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  Lb.Bybit.Announcement in '..\..\lb\Lb.Bybit.Announcement.pas',
  Lb.Bybit.Encryption in '..\..\lb\Lb.Bybit.Encryption.pas',
  Lb.Bybit.SysUtils in '..\..\lb\Lb.Bybit.SysUtils.pas',
  Lb.Bybit.Kline in '..\..\lb\market\Lb.Bybit.Kline.pas',
  Lb.Bot.Tiket in '..\..\lb\Lb.Bot.Tiket.pas',
  Lb.SysUtils.Candel in '..\..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Logger in '..\..\..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  {$IFDEF DEBUG}
  TLogger.ClearLog;
  {$ENDIF}
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
