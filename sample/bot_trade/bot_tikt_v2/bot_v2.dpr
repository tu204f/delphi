program bot_v2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {FormMain},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Bot in 'lb\Lb.Bot.pas',
  UnitLineFrame in 'lb\UnitLineFrame.pas' {LineFrame: TFrame},
  Lb.Line in 'lb\Lb.Line.pas',
  UnitBlocksFrame in 'lb\UnitBlocksFrame.pas' {BlocksFrame: TFrame},
  Lb.Blocks in 'lb\Lb.Blocks.pas',
  UnitBlockFrame in 'lb\UnitBlockFrame.pas' {BlockFrame: TFrame},
  Lb.Trades in 'lb\Lb.Trades.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
