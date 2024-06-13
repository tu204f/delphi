unit Lb.NeuronNet.Files;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.NeuronNet.Neuron;

type
  TParamNeuronNet = class(TObject)
  private
    FData: TStrings;
    FNeuronNet: TNeuronNet;
  protected
    procedure SetNeuronNet;
    procedure SetNeuronNetLayer(ALayer: TLayer);
    procedure SetNeuronNetNeuron(ANeuron: TNeuron);
  protected
    FDataIndex: Integer;
    function GetInc: Integer;
    procedure SetLoadNeuronNet;
    procedure SetLoadWeightLink;
  public
    constructor Create(ANeuronNet: TNeuronNet); virtual;
    destructor Destroy; override;
  public
    procedure Load(const AFileName: String);
    procedure Save(const AFileName: String);
  end;

implementation

{ TParamNeuronNet }

constructor TParamNeuronNet.Create(ANeuronNet: TNeuronNet);
begin
  FNeuronNet := ANeuronNet;
  FData := TStringList.Create;
end;

destructor TParamNeuronNet.Destroy;
begin
  FNeuronNet := nil;
  FreeAndNil(FData);
  inherited;
end;

procedure TParamNeuronNet.Load(const AFileName: String);
begin
  FDataIndex := 0;
  FNeuronNet.Layers.Clear;
  FData.LoadFromFile(AFileName);
  SetLoadNeuronNet;
end;

procedure TParamNeuronNet.Save(const AFileName: String);
begin
  SetNeuronNet;
  FData.SaveToFile(AFileName);
end;

procedure TParamNeuronNet.SetNeuronNet;
var
  xS: String;
  xLayer: TLayer;
  i, iCount: Integer;
begin
  FData.Clear;
  iCount := FNeuronNet.Layers.Count;
  if iCount > 0 then
  begin

    xS := 'neuron_net=';
    for i := 0 to iCount - 2 do
    begin
      xLayer := FNeuronNet.Layers[i];
      xS := xS + (xLayer.Neurons.Count - 1).ToString + ',';
    end;
    xLayer := FNeuronNet.Layers[iCount - 1];
    xS := xS + xLayer.Neurons.Count.ToString;
    FData.Add(xS);


    for i := 0 to iCount - 1 do
    begin
      xLayer := FNeuronNet.Layers[i];
      SetNeuronNetLayer(xLayer);
    end;

  end;
end;

procedure TParamNeuronNet.SetNeuronNetLayer(ALayer: TLayer);
var
  xNeuron: TNeuron;
  i, iCount: Integer;
begin
  iCount := ALayer.Neurons.Count;
  if iCount > 0 then
  begin
    FData.Add('begin(layer)');
    for i := 0 to iCount - 1 do
    begin
      xNeuron := ALayer.Neurons[i];
      SetNeuronNetNeuron(xNeuron);
    end;
    FData.Add('end(layer)');
  end;
end;

procedure TParamNeuronNet.SetNeuronNetNeuron(ANeuron: TNeuron);
var
  xLink: TLink;
  i, iCount: Integer;
begin
  FData.Add('begin(neuron)');
  iCount := ANeuron.Links.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xLink := ANeuron.Links[i];
      FData.Add(' ' + xLink.Weight.ToString);
    end;
  FData.Add('end(neuron)');
end;

function TParamNeuronNet.GetInc: Integer;
begin
  FDataIndex := FDataIndex + 1;
  Result := FDataIndex;
end;

procedure TParamNeuronNet.SetLoadNeuronNet;

  function _Parser(S: String): String;
  var
    xPosInd: Integer;
  begin
    xPosInd := Pos('=',S);
    Result := Copy(S,xPosInd + 1,Length(S) - xPosInd);
  end;

var
  xS: String;
  xStr: TStrings;
  i, iCount: Integer;
  xNumber: String;
begin
  xS := _Parser(FData[FDataIndex]);
  FDataIndex := GetInc;

  xStr := TStringList.Create;
  try
    xStr.Delimiter := ',';
    xStr.DelimitedText := xS;

    iCount := xStr.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 2 do
      begin
        xNumber := xStr[i];
        FNeuronNet.AddLayer(xNumber.ToInteger);
      end;
      xNumber := xStr[iCount - 1];
      FNeuronNet.OutputLayer(xNumber.ToInteger);
      SetLoadWeightLink;
    end;

  finally
    FreeAndNil(xStr);
  end;
end;

procedure TParamNeuronNet.SetLoadWeightLink;

  function _IsBeginLayer(const S: String): Boolean;
  begin
    Result := SameText(Trim(S),'begin(layer)');
  end;

  function _IsBeginNeuron(const S: String): Boolean;
  begin
    Result := SameText(Trim(S),'begin(neuron)');
  end;

  function _IsEndLayer(const S: String): Boolean;
  begin
    Result := SameText(Trim(S),'end(layer)');
  end;

  function _IsEndNeuron(const S: String): Boolean;
  begin
    Result := SameText(Trim(S),'end(neuron)');
  end;

  function GetFloatToStr(S: String): Double;
  var
    xS: String;
  begin
    xS := Trim(S);
    Result := StrToFloat(xS);
  end;

var
  xS: String;
  xLayer: TLayer;
  xNeuron: TNeuron;
  xLink: TLink;
  xIndexLayer, xIndexNeuron, xIndexLink: Integer;
begin
  xLayer := nil;
  xIndexLayer := 0;
  while FDataIndex < FData.Count do
  begin
    xS := FData[FDataIndex];
    FDataIndex := GetInc;

    if _IsBeginLayer(xS) then
    begin
      xLayer := FNeuronNet.Layers[xIndexLayer];
      xIndexNeuron := 0;
      Continue;
    end;

    if _IsEndLayer(xS) then
    begin
      Inc(xIndexLayer);
      Continue;
    end;

    if _IsBeginNeuron(xS) then
    begin
      xIndexLink := 0;
      xNeuron := xLayer.Neurons[xIndexNeuron];
      Continue;
    end;

    if _IsEndNeuron(xS) then
    begin
      Inc(xIndexNeuron);
      Continue;
    end;

    if not Assigned(xLayer) then
      raise Exception.Create('Error Message: Слой не определен');

    if not Assigned(xNeuron) then
      raise Exception.Create('Error Message: Нейрон не определен');

    xLink := xNeuron.Links[xIndexLink];
    xLink.Weight := GetFloatToStr(xS);
    Inc(xIndexLink);

  end;
end;

end.
