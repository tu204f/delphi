program XOR_Problem;

uses
  Forms,
  BackProp in 'BackProp.pas' {Form1};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
