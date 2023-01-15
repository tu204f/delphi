program exmq;

uses
  Vcl.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form4},
  MQ.DataModule in 'MQ.DataModule.pas' {DataModule1: TDataModule};

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm4, Form4);
  Application.CreateForm(TDataModule1, DataModule1);
  Application.Run;
end.
