program trade_bot;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {MainForm},
  BotTrade in 'BotTrade.pas',
  Lb.Candel.Source in 'src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in 'src\Lb.Candel.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
