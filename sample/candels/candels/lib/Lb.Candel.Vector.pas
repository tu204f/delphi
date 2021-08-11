unit Lb.Candel.Vector;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.Candel.SysUtils;

type
  ///<summary>Базовый вектор</summary>
  TCustomVector = class(TObject)
  private
    FValues: TVectorCandelList;
    FResults: TVectorCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Определяющий вектор</summary>
    property Values: TVectorCandelList read FValues;
    ///<summary>Результирующий вектор</summary>
    property Results: TVectorCandelList read FResults;
  end;

  ///<summary>Вектор - объект</summary>
  TVector = class(TCustomVector)
  private
    FID: String;
  public
    constructor Create; override;
    destructor Destroy; override;
    ///<summary>Ключ вектора</summary>
    property ID: String read FID write FID;
  end;
  TVectorList = TObjectList<TVector>;

  ///<summary>Объект конвертируем объект</summary>
  TEnvelopeVector = class(TCustomVector)
  private
    FBaseCandel: TCandel;
  public
    constructor Create; override;
    destructor Destroy; override;
    procedure SetCandels(const AIndexBase, ACountValue, ACountResult: Integer; const ACandels: TCandelList);
    procedure SetWrite;
  public
    property BaseCandel: TCandel read FBaseCandel;
  end;

  ///<summary>Список векторов</summary>
  TVectors = class(TObject)
  private
    FItems: TVectorList;
    FResults: TVectorList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetCurrentVector(const AValues: TVectorCandelList);
    property Items: TVectorList read FItems write FItems;
  end;

implementation

{$IFDEF DEBUG}
  {$DEFINE DB_VECTOR}
{$ENDIF}

uses
  Lb.Logger, Lb.Candel.DB;

{ TCustomVector }

constructor TCustomVector.Create;
begin
  FValues := TVectorCandelList.Create;
  FResults := TVectorCandelList.Create;
end;

destructor TCustomVector.Destroy;
begin
  FreeAndNil(FResults);
  FreeAndNil(FValues);
  inherited;
end;

{ TVector }

constructor TVector.Create;
begin
  inherited;

end;

destructor TVector.Destroy;
begin

  inherited;
end;

{ TEnvelopeVector }

constructor TEnvelopeVector.Create;
begin
  inherited Create;
  FillChar(FBaseCandel,SizeOf(TCandel),0);
end;

destructor TEnvelopeVector.Destroy;
begin
  inherited;
end;

procedure TEnvelopeVector.SetCandels(const AIndexBase, ACountValue,
  ACountResult: Integer; const ACandels: TCandelList);

  function GetValues: Boolean;
  var
    xCandel: TCandel;
    xVectorCandel: TVectorCandel;
    xLow, xHigh: Integer;
  begin
    Result := False;
    xHigh := AIndexBase;
    xLow := xHigh - ACountValue;
    if (xLow >= 0) and (xHigh <= ACandels.Count - ACountResult) then
    begin
      Result := True;
      for var i := xLow to xHigh do
      begin
        xCandel := ACandels[i];
        xVectorCandel := GetVectorValueToCandel(xCandel,FBaseCandel.Close,FBaseCandel.Vol);
        FValues.Add(xVectorCandel);
      end;
    end;
  end;


  function GetResults: Boolean;
  var
    xCandel: TCandel;
    xVectorCandel: TVectorCandel;
    xLow, xHigh: Integer;
  begin
    Result := False;
    xLow := AIndexBase + 1;
    xHigh := xLow + (ACountResult - 1);
    if (xLow >= 0) and (xHigh < ACandels.Count) then
    begin
      Result := True;
      for var i := xLow to xHigh do
      begin
        xCandel := ACandels[i];
        xVectorCandel := GetVectorValueToCandel(xCandel,FBaseCandel.Close,FBaseCandel.Vol);
        FResults.Add(xVectorCandel);
      end;
    end;
  end;

begin
  if (ACountValue <= 0) then
    raise Exception.Create('Error Message: Количество веторок - результетов');
  if (ACountResult <= 0) then
    raise Exception.Create('Error Message: Количество векторов - результата');
  if (AIndexBase < 0) and (AIndexBase < ACountValue) then
    raise Exception.Create('Error Message: Занчение базового ветора не может быть меньше ACountValues');
  FValues.Clear;
  FResults.Clear;
  FBaseCandel := ACandels[AIndexBase];
  if GetValues then
    GetResults;
end;

procedure TEnvelopeVector.SetWrite;
var
  iCount: Integer;
begin
  if not GetIsVector(FBaseCandel.Date,FBaseCandel.Time) then
  begin
    // Обезательно должно быть результирующий веторо
    // и определяющий вектор
    if (FValues.Count > 0) and (FResults.Count > 0) then
    begin
      var xID := GetInsertVector(FBaseCandel.Date,FBaseCandel.Time);
      if not xID.IsEmpty then
      begin
        // Сохроняем значение
        iCount := FValues.Count;
        for var i := 0 to iCount - 1 do
        begin
          var xV := FValues[i];
          SetInsertVectorValue(xID,0,xV.Open,xV.High,xV.Low,xV.Close,xV.Vol);
        end;
        // Сохроняем результат
        iCount := FResults.Count;
        for var i := 0 to iCount - 1 do
        begin
          var xV := FResults[i];
          SetInsertVectorValue(xID,1,xV.Open,xV.High,xV.Low,xV.Close,xV.Vol);
        end;
      end;
    end;
  end;
end;

{ TVectors }

constructor TVectors.Create;
begin
  FItems := TVectorList.Create;
  FResults := TVectorList.Create;
end;

destructor TVectors.Destroy;
begin
  FreeAndNil(FResults);
  FreeAndNil(FItems);
  inherited;
end;

procedure TVectors.SetCurrentVector(const AValues: TVectorCandelList);

  function GetSameVector(const AValues1, AValues2: TVectorCandelList): Boolean;
  var
    xV1: TVectorCandel;
    xV2: TVectorCandel;
    i, Count: Integer;
  begin
    Result := False;
    if (AValues1.Count > 0) and (AValues1.Count = AValues2.Count) then
    begin
      Count := AValues1.Count;
      if Count > 0 then
        for i := 0 to Count - 1 do
        begin
          xV1 := AValues1[i];
          xV2 := AValues2[i];



        end;
    end;
  end;

var
  xVector: TVector;
  i, Count: Integer;
begin
  // Текущий вектор
  Count := FItems.Count;
  if Count > 0 then
    for i := 0 to Count - 1 do
    begin
      xVector := FItems[i];



    end;
end;

end.
