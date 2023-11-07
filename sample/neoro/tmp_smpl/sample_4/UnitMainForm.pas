unit UnitMainForm;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants,
  FMX.Types, FMX.Controls, FMX.Forms, FMX.Graphics, FMX.Dialogs, FMX.Layouts,
  System.Generics.Collections,
  UnitChartCandelsFrame,
  Lb.SysUtils.Candel,
  Lb.Block,
  Lb.NeuronNet,
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ListBox,
  FMX.Memo.Types, FMX.ScrollBox, FMX.Memo;

type
  TIntegerList = TList<Integer>;

  TMainForm = class(TForm)
    Button1: TButton;
    Button2: TButton;
    Button3: TButton;
    Button4: TButton;
    Memo1: TMemo;
    Timer1: TTimer;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    Label6: TLabel;
    Label7: TLabel;
    Label8: TLabel;
    Label9: TLabel;
    Label10: TLabel;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure Button3Click(Sender: TObject);
    procedure Button4Click(Sender: TObject);
    procedure Timer1Timer(Sender: TObject);
  private
    { Private declarations }
  public
    Blocks: TBlockList;

    NeuronNet1 : TNeuronNetThread;
//    NeuronNet2 : TNeuronNetThread;
//    NeuronNet3 : TNeuronNetThread;
//    NeuronNet4 : TNeuronNetThread;
//    NeuronNet5 : TNeuronNetThread;
//    NeuronNet6 : TNeuronNetThread;
//    NeuronNet7 : TNeuronNetThread;
//    NeuronNet8 : TNeuronNetThread;
//    NeuronNet9 : TNeuronNetThread;
//    NeuronNet10: TNeuronNetThread;

    procedure NeuronNetOnBeginLearn(Sender: TObject);
    procedure NeuronNetOnEndLearn(Sender: TObject);
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

procedure TMainForm.FormCreate(Sender: TObject);

  function _CreateNeuronNet(const AFileName: String): TNeuronNetThread;
  begin
    Result := TNeuronNetThread.Create;
    Result.FileName := AFileName;
  end;

begin
  Blocks := TBlockList.Create;

  NeuronNet1 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet2 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet3 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet4 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet5 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet6 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet7 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet8 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet9 := _CreateNeuronNet('neoron1.nr');
//  NeuronNet10:= _CreateNeuronNet('neoron1.nr');

end;

procedure TMainForm.Button1Click(Sender: TObject);

  procedure _ParamNeuronNet(const ANeuronNet: TNeuronNetThread);
  begin
    ANeuronNet.CompileNetWork([751,51,49,10]);
    ANeuronNet.Blocks := Blocks;
  end;

begin

  _ParamNeuronNet(NeuronNet1);
//  _ParamNeuronNet(NeuronNet2);
//  _ParamNeuronNet(NeuronNet3);
//  _ParamNeuronNet(NeuronNet4);
//  _ParamNeuronNet(NeuronNet5);
//  _ParamNeuronNet(NeuronNet6);
//  _ParamNeuronNet(NeuronNet7);
//  _ParamNeuronNet(NeuronNet8);
//  _ParamNeuronNet(NeuronNet9);
//  _ParamNeuronNet(NeuronNet10);

end;

procedure TMainForm.Button2Click(Sender: TObject);
var
  xFN: String;
  xS: String;
  xBlockStr: TStrings;
  xSource: TStrings;
  xBlock: TBlock;
begin
  xFN := 'SPFB.SBRF.csv';
  xSource   := TStringList.Create;
  xBlockStr := TStringList.Create;
  try
    xSource.LoadFromFile(xFN);
    for var i := 0 to xSource.Count - TNeuronNetThread.SIZE_BLOCK - 1 do
    begin
      xBlockStr.Clear;
      for var j := 0 to TNeuronNetThread.SIZE_BLOCK - 1 do
      begin
        xS := xSource[i + j];
        xBlockStr.Add(xS);
      end;
      xBlock := TBlock.Create;
      xBlock.SetParserBlock(xBlockStr);
      Blocks.Add(xBlock);
    end;
  finally
    FreeAndNil(xBlockStr);
    FreeAndNil(xSource);
  end;
  ShowMessage('–азбиты на блоки: ' + Blocks.Count.ToString);
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Blocks);
end;

procedure TMainForm.Button3Click(Sender: TObject);
begin
  NeuronNet1.StartLearn;
//  NeuronNet2.StartLearn;
//  NeuronNet3.StartLearn;
//  NeuronNet4.StartLearn;
//  NeuronNet5.StartLearn;
//  NeuronNet6.StartLearn;
//  NeuronNet7.StartLearn;
//  NeuronNet8.StartLearn;
//  NeuronNet9.StartLearn;
//  NeuronNet10.StartLearn;
end;

procedure TMainForm.Button4Click(Sender: TObject);
begin
  NeuronNet1.StopLearn;
//  NeuronNet2.StopLearn;
//  NeuronNet3.StopLearn;
//  NeuronNet4.StopLearn;
//  NeuronNet5.StopLearn;
//  NeuronNet6.StopLearn;
//  NeuronNet7.StopLearn;
//  NeuronNet8.StopLearn;
//  NeuronNet9.StopLearn;
//  NeuronNet10.StopLearn;
end;

procedure TMainForm.NeuronNetOnBeginLearn(Sender: TObject);
begin
  // —обытие начало обучение
  Memo1.Lines.Add('NeuronNetOnBeginLearn');
end;

procedure TMainForm.NeuronNetOnEndLearn(Sender: TObject);
begin
  // —обытие остановик обучение
  Memo1.Lines.Add('NeuronNetOnEndLearn');
end;

procedure TMainForm.Timer1Timer(Sender: TObject);


begin
  Label1.Text :=
    Format('Ёпоха: %d; Progress Block: %d',
      [NeuronNet1.Status.CountLearn,NeuronNet1.Status.CountBlocks]);

//  Label2.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet2.Status.CountLearn,NeuronNet2.Status.CountBlocks]);
//
//  Label3.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet3.Status.CountLearn,NeuronNet3.Status.CountBlocks]);
//
//  Label4.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet3.Status.CountLearn,NeuronNet4.Status.CountBlocks]);
//
//  Label5.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet5.Status.CountLearn,NeuronNet5.Status.CountBlocks]);
//
//  Label6.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet6.Status.CountLearn,NeuronNet6.Status.CountBlocks]);
//
//  Label7.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet7.Status.CountLearn,NeuronNet7.Status.CountBlocks]);
//
//  Label8.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet8.Status.CountLearn,NeuronNet8.Status.CountBlocks]);
//
//  Label9.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet9.Status.CountLearn,NeuronNet9.Status.CountBlocks]);
//
//  Label10.Text :=
//    Format('Ёпоха: %d; Progress Block: %d',
//      [NeuronNet10.Status.CountLearn,NeuronNet10.Status.CountBlocks]);

end;

end.
