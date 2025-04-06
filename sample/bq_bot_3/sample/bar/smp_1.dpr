program smp_1;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  UnitBarCandelFrame in '..\..\lb\bar\UnitBarCandelFrame.pas' {BarFrame: TFrame};

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm4, Form4);
  Application.Run;
end.
