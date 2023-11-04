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
  FMX.Controls.Presentation, FMX.StdCtrls, FMX.Objects, FMX.ListBox;

type
  TMainForm = class(TForm)
    Layout: TLayout;
    ButtonCreateBlocks: TButton;
    ButtonRandomBlock: TButton;
    Text1: TText;
    Text2: TText;
    ListBox: TListBox;
    ListBoxEtalon: TListBox;
    ButtonLoad: TButton;
    procedure FormCreate(Sender: TObject);
    procedure FormClose(Sender: TObject; var Action: TCloseAction);
    procedure ButtonCreateBlocksClick(Sender: TObject);
    procedure ButtonRandomBlockClick(Sender: TObject);
    procedure ButtonLoadClick(Sender: TObject);
  private
    { Private declarations }
    procedure SetAnticipation(ABlock: TBlock);
  public
    Blocks: TBlockList;
    ChartCandelsFrame: TChartCandelsFrame;
    function Block(const AIndexBlock: Integer): Boolean;
  end;

var
  MainForm: TMainForm;

implementation

{$R *.fmx}

const
  SIZE_BLOCK = 160;
  SIZE_F_BLOCK = 10;

type
  TIntegerList = TList<Integer>;

var
  NeuronNet: TNeuronNet = nil;
  Etalons: TIntegerList;


procedure NNet_SetBlock(ABlock: TBlock);
var
  xCandel: TCandel;
  xInd, i: Integer;
begin
  // Записываем входящий значение
  xInd := 0;
  for i := 0 to ABlock.Candels.Count - SIZE_F_BLOCK - 1 do
  begin
    xCandel := ABlock.Candels[i];

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Open;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.High;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Low;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Close;
    Inc(xInd);

    NeuronNet.InputNeurons.Values[xInd] := xCandel.Vol;
    Inc(xInd);

  end;
  NeuronNet.InputNeurons.Values[xInd] := 1;
  NeuronNet.Calculate;
end;

function NNet_OutputEtalon(ABlock: TBlock): Integer;
var
  xPr, xD: Integer;
  xLast, xPrice: Double;
begin
  // Возращает выриант ответа
  xLast := ABlock.Candels[ABlock.Candels.Count - 1].Close;
  xPrice := ABlock.Candels[ABlock.Candels.Count - SIZE_F_BLOCK - 1].Close;

  xPr := Trunc(100 *(xLast/xPrice));

  if xPr > 100 then
  begin
    xD := xPr - 100;
    if (xD >= 0) and (xD < 5) then
      Result := 5
    else if (xD >= 5) and (xD < 10) then
      Result := 6
    else if (xD >= 15) and (xD < 20) then
      Result := 7
    else if (xD >= 20) and (xD < 25) then
      Result := 8
    else
      Result := 9
  end
  else
  begin
    xD := 100 - xPR;
    if (xD >= 0) and (xD < 5) then
      Result := 4
    else if (xD >= 5) and (xD < 10) then
      Result := 3
    else if (xD >= 15) and (xD < 20) then
      Result := 2
    else if (xD >= 20) and (xD < 25) then
      Result := 1
    else
      Result := 0
  end;

end;

procedure NNet_Etalon(ABlock: TBlock);
begin
  NeuronNet.OutputNeurons.EtalonNullValue;
  MainForm.SetAnticipation(ABlock);

  for var i := 0 to Etalons.Count - 1 do
    NeuronNet.OutputNeurons.Errors[i] := Etalons[i];
end;


procedure TMainForm.FormCreate(Sender: TObject);
begin
  Blocks := TBlockList.Create;

  ChartCandelsFrame := TChartCandelsFrame.Create(nil);
  ChartCandelsFrame.Parent := Layout;
  ChartCandelsFrame.Align := TAlignLayout.Client;

  NeuronNet := TNeuronNet.Create;
  NeuronNet.CompileNetWork([751,51,49,10]);

  Etalons := TIntegerList.Create;
end;

procedure TMainForm.SetAnticipation(ABlock: TBlock);

  procedure _SetMaxMinValue(const AValue: Integer; var AMaxValue, AMinValue: Integer);
  begin
    if AValue > AMaxValue then
      AMaxValue := AValue;
    if AValue < AMinValue then
      AMinValue := AValue;
  end;

  procedure _ListBoxEtalon;
  begin
    Etalons.Clear;
    ListBoxEtalon.Items.Clear;
    for var i := 0 to SIZE_F_BLOCK - 1 do
    begin
      Etalons.Add(0);
      ListBoxEtalon.Items.Add('0');
    end;
  end;

var
  xLast, xHigh, xLow, xValue: Integer;
  xIndex: Integer;
  xCount: Integer;
  xCandel: TCandel;
var
  xInd: Integer;
  xMaxValue, xMinValue: Integer;
begin
  xMaxValue := 0;
  xMinValue := 100;

  ListBoxEtalon.BeginUpdate;
  try
    _ListBoxEtalon;

    xIndex := ABlock.Candels.Count - SIZE_F_BLOCK;
    xCount := ABlock.Candels.Count;

    xLast := Trunc(100 * ABlock.Candels[xIndex].Close);
    for var i := xIndex to xCount - 1 do
    begin
      xHigh := Trunc(100 * ABlock.Candels[i].High);
      xLow  := Trunc(100 * ABlock.Candels[i].Low);
      xValue := (xHigh - xLast);
      _SetMaxMinValue(xValue,xMaxValue, xMinValue);
      xValue := (xLow - xLast);
      _SetMaxMinValue(xValue,xMaxValue, xMinValue);
    end;

    for var i := 0 to 4 do
    begin
      xInd := 4 - i;
      // --------------------------------
      if (10 * i) <= xMaxValue then
      begin
        ListBoxEtalon.Items[xInd] := '1.' + xMaxValue.ToString;
        Etalons[xInd] := 1;
      end;
      // --------------------------------
      xInd := i + 5;
      if (-10 * i) >= xMinValue then
      begin
        ListBoxEtalon.Items[xInd] := '1.' + xMinValue.ToString;
        Etalons[xInd] := 1;
      end;
    end;

  finally
    ListBoxEtalon.EndUpdate;
  end;
end;

procedure TMainForm.FormClose(Sender: TObject; var Action: TCloseAction);
begin
  FreeAndNil(Etalons);
  FreeAndNil(NeuronNet);
  FreeAndNil(ChartCandelsFrame);
  FreeAndNil(Blocks);
end;

function TMainForm.Block(const AIndexBlock: Integer): Boolean;
var
  xBlock: TBlock;
  xLast, xPrice: Double;
begin
  Result := False;
  if (AIndexBlock >= 0) and (AIndexBlock < Blocks.Count) then
  begin
    xBlock := Blocks[AIndexBlock];
    ChartCandelsFrame.Candels.Copy(xBlock.Candels);
    ChartCandelsFrame.BuildChart;
    Result := True;
  end;

  xLast := xBlock.Candels[xBlock.Candels.Count - 1].Close;
  xPrice := xBlock.Candels[xBlock.Candels.Count - SIZE_F_BLOCK - 1].Close;

  Text1.Text :=
    'Price:' + Trunc(100 * xPrice).ToString + ' ' +
    'Last:' + Trunc(100 * xLast).ToString + ' ' +
    'Delta: ' + Trunc(100 * (xLast - xPrice)).ToString;
end;

procedure TMainForm.ButtonCreateBlocksClick(Sender: TObject);
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
    for var i := 0 to xSource.Count - SIZE_BLOCK - 1 do
    begin

      xBlockStr.Clear;
      for var j := 0 to SIZE_BLOCK - 1 do
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
  ShowMessage('Разбиты на блоки: ' + Blocks.Count.ToString);
end;


procedure TMainForm.ButtonLoadClick(Sender: TObject);
begin
  // Загурзка
  NeuronNet.Load('neoron1.nr');
end;

function OutputIndexNeuron: Integer;
var
  xV: Double;
  i, N: Integer;
  Max: Single;
begin

  MainForm.ListBox.Items.Clear;

  N := 0;
  Max := 0.0;
  for i := 0 to NeuronNet.OutputNeurons.Count - 1 do
  begin
    // -------------------------------------
    xV := NeuronNet.OutputNeurons.Values[i];
    MainForm.ListBox.Items.Add('i = ' + i.ToString + ' :: ' + xV.ToString);

    if xV > Max then //поиск максимума, классика
    begin
      N := i;
      Max := xV;
    end;
  end;
  Result := N;
end;

procedure TMainForm.ButtonRandomBlockClick(Sender: TObject);
var
  xIndexBlock: Integer;
  xBlock: TBlock;
begin
  xIndexBlock := Random(Blocks.Count);
  Block(xIndexBlock);

  xBlock := Blocks[xIndexBlock];
  NNet_SetBlock(xBlock);


  SetAnticipation(xBlock);

  Text2.Text :=
    OutputIndexNeuron.ToString + ' Какой должен быть:' +
    NNet_OutputEtalon(xBlock).ToString;
end;


end.
