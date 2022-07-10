program test_bot_db;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitTestBotDataBaseMain in 'UnitTestBotDataBaseMain.pas' {MainForm},
  Lb.DataModuleDB in '..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.SysUtils.ISO860 in '..\..\library\Lb.SysUtils.ISO860.pas',
  UnitBotTrades in 'UnitBotTrades.pas',
  BotTrade in 'BotTrade.pas',
  Lb.Candel.Source in 'src\Lb.Candel.Source.pas',
  Lb.Candel.SysUtils in 'src\Lb.Candel.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.Run;
end.
