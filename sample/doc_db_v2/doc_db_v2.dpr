program doc_db_v2;

uses
  System.StartUpCopy,
  FMX.Forms,
  UnitMainForm in 'UnitMainForm.pas' {Form3},
  Lb.DataModuleDB in '..\..\library\db\Lb.DataModuleDB.pas' {DataModuleDB: TDataModule},
  Lb.Document.DataBase in 'Lb.Document.DataBase.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.CreateForm(TForm3, Form3);
  Application.Run;
end.
