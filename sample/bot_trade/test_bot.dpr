program test_bot;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitTestMainForm in 'UnitTestMainForm.pas' {MainForm},
  Lb.Candel.Source in 'src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in 'src\Lb.Candel.SysUtils.pas',
  BotTrade in 'BotTrade.pas',
  Lb.Logger in '..\..\library\Lb.Logger.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
