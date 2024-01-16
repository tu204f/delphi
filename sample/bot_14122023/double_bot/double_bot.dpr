program double_bot;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainFrom in 'UnitMainFrom.pas' {Form4},
  Lb.Bot in '..\lb\Lb.Bot.pas',
  Lb.Bot.Tiket in '..\lb\Lb.Bot.Tiket.pas',
  Lb.ReadPrice in '..\lb\Lb.ReadPrice.pas',
  Lb.SysUtils in '..\lb\Lb.SysUtils.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
