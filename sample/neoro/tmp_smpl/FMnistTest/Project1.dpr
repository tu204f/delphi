program Project1;

uses
  Vcl.Forms,
  Unit1 in 'Unit1.pas' {Form1},
  Lb.Source.DB in '..\lib\Lb.Source.DB.pas',
  Lb.NeuronNet.Neuron in '..\lib\Lb.NeuronNet.Neuron.pas';

{$R *.res}

begin
  Application.Initialize;
  Application.MainFormOnTaskbar := True;
  Application.CreateForm(TForm1, Form1);
  Application.Run;
end.
