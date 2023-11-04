unit Lb.NeuronNet;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Contnrs,
  System.Generics.Collections,
  Lb.ActivationFunction;

type
  TArrInteger = array of Integer;

  ///<summary>�������� �������</summary>
  TNeuronValues = array of Double;

  ///<summary>�������� �����</summary>
  TWeightValues = array of array of Double;

  {todo: ����� � ������ ���� �������� �������� ������}

  ///<summary>������ ��������</summary>
  TNeurons = class(TObject)
  private
    FCount: Integer;
    FValues: TNeuronValues; // �������� ��������
    FErrors: TNeuronValues; // ������ ��������
    function GetValues(Index: Integer): Double;
    procedure SetValues(Index: Integer; const Value: Double);
    function GetErrors(Index: Integer): Double;
    procedure SetErrors(Index: Integer; const Value: Double);
  public
    constructor Create(const ACount: Integer); virtual;
    destructor Destroy; override;
    ///<summary>��������� �������</summary>
    property Values[Index: Integer]: Double read GetValues write SetValues;
    ///<summary>������ �������� ������</summary>
    property Errors[Index: Integer]: Double read GetErrors write SetErrors;
    ///<summary>���������� ��������</summary>
    property Count: Integer read FCount;
  end;

  ///<summary>��������� �������� ��������� �������</summary>
  TOutputNeurons = class(TNeurons)
  private
    FEtalons: TNeuronValues;
    function GetEtalons(Index: Integer): Double;
    procedure SetEtalons(Index: Integer; const Value: Double);
  public
    constructor Create(const ACount: Integer); override;
    destructor Destroy; override;
    ///<summary>�������� �������� �������</summary>
    procedure EtalonNullValue;
    ///<summary>��������� ���������� �� ��������� �� ����������</summary>
    procedure CalculateError;
    ///<summary>������ �������� ������</summary>
    property Etalons[Index: Integer]: Double read GetEtalons write SetEtalons;
  end;

  ///<summary>�������� �����</summary>
  TWeights = class(TObject)
  private
    FValues: TWeightValues;
    FInputNeurons, FOutputNeurons: TNeurons;
    function GetCheck(AInputIndex, AOutputIndex: Integer): Boolean;
    function GetCells(AInputIndex, AOutputIndex: Integer): Double;
    procedure SetCells(AInputIndex, AOutputIndex: Integer; const Value: Double);
  public
    constructor Create(const AInputNeurons, AOutputNeurons: TNeurons); virtual;
    destructor Destroy; override;
    procedure RandomWeight;
    property Cells[AInputIndex, AOutputIndex: Integer]: Double read GetCells write SetCells;
  public
    property InputNeurons: TNeurons read FInputNeurons;
    property OutputNeurons: TNeurons read FOutputNeurons;
  end;

  ///<summary>���������� ����������� �������� �� ������� ������</summary>
  TNeuronNet = class(TObject)
  public const
    VER_NUMBER = 1; // ����� ������
    VER_BILD   = 1; // ����� ������
  private
    FTypeFuction: TTypeFuction;
    FObjects: TObjectList;
    function GetInputNeurons: TNeurons;
    function GetOutputNeurons: TOutputNeurons;
    function GetWeights(Index: Integer): TWeights;
    function GetWeightsCount: Integer;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure CompileNetWork(const ACounts: TArrInteger);

    procedure Calculate;
    procedure CalculateError;
    procedure CalculateLearn(AValue: Double);

    property InputNeurons: TNeurons read GetInputNeurons;
    property OutputNeurons: TOutputNeurons read GetOutputNeurons;
    property Weights[Index: Integer]: TWeights read GetWeights;
    property WeightsCount: Integer read GetWeightsCount;

    property TypeFuction: TTypeFuction read FTypeFuction write FTypeFuction;
  public
    procedure Save(const AFileName: String);
    procedure Load(const AFileName: String);
  end;

implementation

{ TNeurons }

constructor TNeurons.Create(const ACount: Integer);
begin
  FCount := ACount;
  SetLength(FValues,FCount);
  SetLength(FErrors,FCount);
end;

destructor TNeurons.Destroy;
begin
  SetLength(FErrors,0);
  SetLength(FValues,0);
  inherited;
end;

function TNeurons.GetValues(Index: Integer): Double;
begin
  if (Index >=0) and (Index < FCount) then
    Result := FValues[Index]
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� ��������');
end;

procedure TNeurons.SetValues(Index: Integer; const Value: Double);
begin
  if (Index >=0) and (Index < FCount) then
    FValues[Index] := Value
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� ��������');
end;

function TNeurons.GetErrors(Index: Integer): Double;
begin
  if (Index >=0) and (Index < FCount) then
    Result := FErrors[Index]
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� (������) ��������');
end;

procedure TNeurons.SetErrors(Index: Integer; const Value: Double);
begin
  if (Index >=0) and (Index < FCount) then
    FErrors[Index] := Value
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� (������) ��������');
end;

{ TOutputNeurons }

constructor TOutputNeurons.Create(const ACount: Integer);
begin
  inherited Create(ACount);
  SetLength(FEtalons,ACount);
end;

destructor TOutputNeurons.Destroy;
begin
  SetLength(FEtalons,0);
  inherited;
end;

procedure TOutputNeurons.EtalonNullValue;
var
  i: Integer;
begin
  for i := 0 to FCount - 1  do
    FEtalons[i] := 0;
end;

function TOutputNeurons.GetEtalons(Index: Integer): Double;
begin
  if (Index >=0) and (Index < FCount) then
    Result := FEtalons[Index]
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� (������) ��������');
end;

procedure TOutputNeurons.SetEtalons(Index: Integer; const Value: Double);
begin
  if (Index >=0) and (Index < FCount) then
    FEtalons[Index] := Value
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� (������) ��������');
end;

procedure TOutputNeurons.CalculateError;
var
  i: Integer;
  xEr, xEt, xVl: Double;
begin
  if FCount > 0 then
    for i := 0 to FCount - 1  do
    begin
      xEt := FEtalons[i];
      xVl := FValues[i];
      xEr := xEt - xVl;
      FErrors[i] := xEr;
    end;
end;

{ TWeights }

constructor TWeights.Create(const AInputNeurons, AOutputNeurons: TNeurons);
begin
  FInputNeurons := AInputNeurons;
  FOutputNeurons:= AOutputNeurons;
  SetLength(FValues, FInputNeurons.Count, FOutputNeurons.Count);
end;

destructor TWeights.Destroy;
begin

  inherited;
end;

function TWeights.GetCheck(AInputIndex, AOutputIndex: Integer): Boolean;
begin
  Result := (AInputIndex >= 0)  and (AInputIndex  < FInputNeurons.Count) and
            (AOutputIndex >= 0) and (AOutputIndex < FOutputNeurons.Count);
end;

procedure TWeights.RandomWeight;
begin
  Randomize;
  for var I := 0 to FInputNeurons.Count - 1 do
    for var j := 0 to FOutputNeurons.Count - 1 do
      FValues[i,j] := (Random(201) - 100) / 100;
end;

function TWeights.GetCells(AInputIndex, AOutputIndex: Integer): Double;
begin
  if GetCheck(AInputIndex, AOutputIndex) then
    Result := FValues[AInputIndex, AOutputIndex]
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� �����');
end;

procedure TWeights.SetCells(AInputIndex, AOutputIndex: Integer; const Value: Double);
begin
  if GetCheck(AInputIndex, AOutputIndex) then
    FValues[AInputIndex, AOutputIndex] := Value
  else
    raise Exception.Create('Error Message: ����� �� ������� ������� �����');
end;

{ TNeuronNet }

constructor TNeuronNet.Create;
begin
  FTypeFuction := TTypeFuction.tfSigma;
  FObjects := TObjectList.Create;
end;

destructor TNeuronNet.Destroy;
begin
  FreeAndNil(FObjects);
  inherited;
end;


procedure TNeuronNet.CompileNetWork(const ACounts: TArrInteger);

  procedure _CreateWeights;
  var
    xIndex: Integer;
    xWeights: TWeights;
    xInputNeurons, xOutputNeurons: TNeurons;
  begin
    xIndex := 0;
    while xIndex < FObjects.Count do
    begin
      if (xIndex + 2) >= FObjects.Count then
        Break;

      xInputNeurons  := TNeurons(FObjects[xIndex]);
      xOutputNeurons := TNeurons(FObjects[xIndex + 2]);

      xWeights := TWeights.Create(xInputNeurons,xOutputNeurons);
      xWeights.RandomWeight;

      FObjects[xIndex + 1] := xWeights;

      xIndex := xIndex + 2;
    end;
  end;

var
  i, xL, xH, xCount: Integer;
  xNeurons: TNeurons;
begin
  xL := Low(ACounts);
  xH := High(ACounts);

  FObjects.Clear;
  for i := xL to xH do
  begin
    xCount := ACounts[i];
    if i = xH then
      xNeurons := TOutputNeurons.Create(xCount)
    else
      xNeurons := TNeurons.Create(xCount);
    FObjects.Add(xNeurons);
    FObjects.Add(nil);
  end;
  FObjects.Delete(FObjects.Count - 1);
  _CreateWeights;
end;

procedure TNeuronNet.Calculate;

  procedure _CalculateWeights(AWeights: TWeights);
  var
    i, j: Integer;
    xSum: Double;
    xW, xV: Double;
  begin
    {todo: ��������� ������ �� �������, �� �������� ����������}
    for j := 0 to AWeights.OutputNeurons.Count - 1 do
    begin
      xSum := 0.0;
      for i := 0 to AWeights.InputNeurons.Count - 1 do
      begin
        xW := AWeights.Cells[i,j];
        xV := AWeights.InputNeurons.Values[i];
        xSum := xSum + xW * xV;
      end;
      {todo: ��������� ������ � � ���� ���������}
      AWeights.OutputNeurons.Values[j] := GetEquations(FTypeFuction)(xSum);
    end;
  end;

var
  xIndex: Integer;
  xWeights: TWeights;
begin
  // ���������� �������� ����
  xIndex := 1;
  while xIndex < FObjects.Count do
  begin
    xWeights := TWeights(FObjects[xIndex]);
    _CalculateWeights(xWeights);
    xIndex := xIndex + 2;
    if xIndex >= FObjects.Count then
      Break;
  end;
end;

procedure TNeuronNet.CalculateError;

  procedure _CalculateErrorWeights(AWeights: TWeights);
  var
    i, j: Integer;
    xSum: Double;
    xW, xV: Double;
  begin
    for i := 0 to AWeights.InputNeurons.Count - 1 do
    begin
      xSum := 0.0;
      for j := 0 to AWeights.OutputNeurons.Count - 1 do
      begin
        xW := AWeights.Cells[i,j];
        xV := AWeights.OutputNeurons.Errors[j];
        xSum := xSum + xW * xV;
      end;
      {todo: ��������� ������ � � ���� ���������}
      AWeights.InputNeurons.Errors[i] := xSum * GetDeffEquations(FTypeFuction)(AWeights.InputNeurons.Values[i]);
    end;
  end;


var
  xIndex: Integer;
  xWeights: TWeights;
  xNeurons: TOutputNeurons;
begin
  xIndex := FObjects.Count - 1;

  xNeurons := TOutputNeurons(FObjects[xIndex]);
  xNeurons.CalculateError;

  // ������ � �������� �������
  for var i := 0 to xNeurons.Count - 1 do
    xNeurons.Errors[i] := xNeurons.Errors[i] * GetDeffEquations(FTypeFuction)(xNeurons.Values[i]);

  xIndex := xIndex - 1;
  // ������ ���� ���������
  while xIndex > 1 do
  begin
    xWeights := TWeights(FObjects[xIndex]);
    _CalculateErrorWeights(xWeights);
    xIndex := xIndex - 2;
    if xIndex >= FObjects.Count then
      Break;
  end;
end;

procedure TNeuronNet.CalculateLearn(AValue: Double);

  procedure _CalculateLearnWeights(AWeights: TWeights);
  var
    j, i: Integer;
    xW, xV, xE: Double;
    xDeltaW: Double;
  begin
    for j := 0 to AWeights.OutputNeurons.Count - 1 do
    begin
      for i := 0 to AWeights.InputNeurons.Count - 1 do
      begin
        xW := AWeights.Cells[i,j];
        xE := AWeights.OutputNeurons.Errors[j];
        xV := AWeights.InputNeurons.Values[i];
        xDeltaW := AValue * xE * xV;
        AWeights.Cells[i,j] := xW + xDeltaW;
      end;
    end;
  end;

var
  xIndex: Integer;
  xWeights: TWeights;
begin
  xIndex := 1;
  while xIndex < FObjects.Count do
  begin
    xWeights := TWeights(FObjects[xIndex]);
    _CalculateLearnWeights(xWeights);
    xIndex := xIndex + 2;
    if xIndex >= FObjects.Count then
      Break;
  end;
end;

function TNeuronNet.GetInputNeurons: TNeurons;
begin
  {todo: ������ �� ������}
  Result := TNeurons(FObjects[0]);
end;

function TNeuronNet.GetOutputNeurons: TOutputNeurons;
begin
  {todo: ������ �� ������}
  Result := TOutputNeurons(FObjects[FObjects.Count - 1]);
end;

function TNeuronNet.GetWeights(Index: Integer): TWeights;
var
  xIndex: Integer;
begin
  if Index = 0 then
    xIndex := 1
  else
    xIndex := 2 * Index + 1;
  Result := TWeights(FObjects[xIndex]);
end;

function TNeuronNet.GetWeightsCount: Integer;
begin
  Result := 0;
  if FObjects.Count > 0 then
    Result := FObjects.Count div 2;
end;

procedure TNeuronNet.Save(const AFileName: String);

  procedure _VersionNeuronNet(ASource: TStrings);
  begin
    // ������ ����
    ASource.Add('ver_number=' + VER_NUMBER.ToString);
    ASource.Add('ver_bild=' + VER_BILD.ToString);
  end;

  procedure _ParamNeuronNet(ASource: TStrings);
  var
    xS: String;
    i: Integer;
    xNeurons: TNeurons;
  begin
    // ��������� ���������
    xS := '';
    i := 0;
    while i < FObjects.Count do
    begin
      xNeurons := TNeurons(FObjects[i]);
      xS := xS + xNeurons.Count.ToString + ';';
      i := i + 2;
    end;
    ASource.Add(xS);
  end;

  procedure _SaveWeights(ASource: TStrings; AWeights: TWeights);
  var
    xW: Double;
    i, iCount: Integer;
    j, jCount: Integer;
  begin
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    if (iCount = 0) and (jCount = 0) then
      Exit;
    ASource.Add(Format('begin[%d,%d]',[iCount,jCount]));
    for i := 0 to iCount - 1 do
      for j := 0 to jCount - 1 do
      begin
        xW := AWeights.Cells[i,j];
        ASource.Add(Format('w[%d,%d]=%g',[i,j,xW]));
      end;
    ASource.Add('end');
  end;

var
  i, iCount: Integer;
  xWeights: TWeights;
  xSource: TStrings;
begin
  // ��� ����� ��������� ������ ����, ����� ������
  xSource := TStringList.Create;
  try
    iCount := GetWeightsCount;
    _VersionNeuronNet(xSource);
    _ParamNeuronNet(xSource);
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xWeights := GetWeights(i);
        _SaveWeights(xSource,xWeights);
      end;
    end;
    xSource.SaveToFile(AFileName);
  finally
    FreeAndNil(xSource);
  end;
end;

procedure TNeuronNet.Load(const AFileName: String);

  function GetByValue(S: String): String;
  var
    xPos: Integer;
  begin
    xPos := Pos('=',S) + 1;
    Result := Copy(S,xPos,256);
  end;

  function _StrToFloat(const AValue: String): Double;
  begin
    Result := StrToFloatDef(GetByValue(AValue),0);
  end;

  function _IsVersionNeuronNet(ASource: TStrings): Boolean;
  var
    xS: String;
    xVerNumber: Integer;
  begin
    // ������ �����
    // 0 - ������
    // 1 - ������
    xS := ASource[0];
    xVerNumber := StrToIntDef(GetByValue(xS),-1);
    Result := xVerNumber = VER_NUMBER;
  end;

  procedure _ParamNeuronNet(ASource: TStrings);
  var
    xS, xR: String;
    tmpStr: TStrings;
    xCount: Integer;
    xCounts: TArrInteger;
  begin
    // ��������� ���������
    // 2 - ������
    xS := ASource[2];
    tmpStr := TStringList.Create;
    try
      tmpStr.Delimiter := ';';
      tmpStr.DelimitedText := xS;
      xCount := 0;
      for xR in tmpStr do
      begin
        if not xR.IsEmpty then
        begin
          Inc(xCount);
          SetLength(xCounts,xCount);
          xCounts[xCount - 1] := StrToIntDef(xR,-1);
        end;
      end;
      if xCount > 0 then
        Self.CompileNetWork(xCounts);
    finally
      FreeAndNil(tmpStr);
    end;
  end;

  function _LoadWeights(ASource: TStrings; AWeights: TWeights; APosition: Integer): Integer;
  var
    xS: String;
    xW: Double;
    i, iCount: Integer;
    j, jCount: Integer;
    xPosition: Integer;
  begin
    iCount := AWeights.InputNeurons.Count;
    jCount := AWeights.OutputNeurons.Count;
    if (iCount = 0) and (jCount = 0) then
    begin
      Result := -1;
      Exit;
    end;

    xPosition := APosition;
    xS := ASource[xPosition];

    if Pos('begin',xS) > 0 then
    begin
      Inc(xPosition);
      for i := 0 to iCount - 1 do
        for j := 0 to jCount - 1 do
        begin
          xS := ASource[xPosition];
          xW := _StrToFloat(xS);
          AWeights.Cells[i,j] := xW;
          if xPosition >= ASource.Count then
            raise Exception.Create('Error Message' + sLineBreak +
            'TNeuronNet.Load()._LoadWeights()' + sLineBreak +
            '�� ���������� ��������� ��������� ����.');
          Inc(xPosition);
        end;
    end;

    xS := ASource[xPosition];
    if Pos('end',xS) > 0 then
      Result := xPosition + 1
    else
      raise Exception.Create('Error Message' + sLineBreak +
        'TNeuronNet.Load()._LoadWeights()' + sLineBreak +
        'C�������� ���� � ������� ����� �� ���������');
  end;

var
  xWeights: TWeights;
  xSource: TStrings;
  xPosition: Integer;
begin
  // ��������� ����
  xSource := TStringList.Create;
  try
    xSource.LoadFromFile(AFileName);
    if _IsVersionNeuronNet(xSource) then
    begin
      xPosition := 3;
      _ParamNeuronNet(xSource);
      var xWeightsCount := Self.GetWeightsCount;
      for var i := 0 to xWeightsCount - 1 do
      begin
        xWeights := Self.Weights[i];
        xPosition := _LoadWeights(xSource,xWeights,xPosition);
      end;
    end;
  finally
    FreeAndNil(xSource);
  end;
end;

end.
