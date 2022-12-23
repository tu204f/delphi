program bot_v2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {FormMain},
  Lb.SysUtils.Candel in '..\..\..\library\trade\patern\Lb.SysUtils.Candel.pas',
  Lb.Logger in '..\..\..\library\Lb.Logger.pas',
  Lb.Bot in 'lb\Lb.Bot.pas',
  UnitLineFrame in 'lb\UnitLineFrame.pas' {LineFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TFormMain, FormMain);
  Application.Run;
end.
