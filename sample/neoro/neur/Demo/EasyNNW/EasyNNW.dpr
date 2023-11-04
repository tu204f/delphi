program EasyNNW;

uses
  Forms,
  NeuralNetExtend in 'NeuralNetExtend.pas' {frmNeuralNetExtend};

{$R *.RES}

begin
  Application.Initialize;
  Application.CreateForm(TfrmNeuralNetExtend, frmNeuralNetExtend);
  Application.Run;
end.
