unit Lb.SysUtils.Structure;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  Lb.SysUtils.Candel;

type
  ///<summary>Структура данных, хранить массив свечей</summary>
  TStructure = class(TObject)
  private
    FSources: TCandelList;
    FSourceCount: Integer;
    FVectorCount: Integer;
  public
    constructor Create; virtual;
    constructor CreateConvert(AStructure: TStructure); virtual;
    destructor Destroy; override;
    ///<summary>Опорный вектор</summary>
    property SourceCount: Integer read FSourceCount write FSourceCount;
    property VectorCount: Integer read FVectorCount write FVectorCount;
    property Sources: TCandelList read FSources;
  end;

  ///<summary>
  /// Генерируем количество объектов
  ///</summary>
  TEventStructurePacked = procedure(const AStructure: TStructure) of object;
  TGenerationStructures = class(TObjectList<TStructure>)
  private
    FCapacityCandels: Integer;
    FSourceCount: Integer;
    FVectorCount: Integer;
    procedure SetSourceCount(const Value: Integer);
    procedure SetVectorCount(const Value: Integer);
  protected
    FEventStructurePacked: TEventStructurePacked;
    procedure DoStructurePacked(const AStructure: TStructure);
  public
    constructor Create; overload;
    procedure AddCandel(const ACandel: TCandel);
    property SourceCount: Integer read FSourceCount write SetSourceCount;
    property VectorCount: Integer read FVectorCount write SetVectorCount;
    property OnStructurePacked: TEventStructurePacked write FEventStructurePacked;
  end;

  ///<summary>
  ///Список структура - патернов
  ///</summary>
  TPaternStructures = class(TObjectList<TStructure>)
  protected
  public
    function AddStructure(const AStructure: TStructure): Integer;
  end;

  TStructureAPI = record
    ///<summary>Вычисляем среднею скользящею</summary>
    class function GetSimpleMovingAverages(AStructure: TStructure): Double; static;
    ///<summary>Вычисляем среднею скользящею: Направление совпадает</summary>
    class function IsSimpleMovingAverages(AStructure: TStructure; VectorCount: Integer = 1): Boolean; static;
  end;


implementation

function SameStructureFull(const AStructure1, AStructure2: TStructure): Boolean;

  function _IsCandel(const ACandel1, ACandel2: TCandel; ASensValue: Integer): Boolean;
  begin
    Result := (Abs(ACandel1.Open  - ACandel2.Open)  <= ASensValue) and
              (Abs(ACandel1.High  - ACandel2.High)  <= ASensValue) and
              (Abs(ACandel1.Low   - ACandel2.Low)   <= ASensValue) and
              (Abs(ACandel1.Close - ACandel2.Close) <= ASensValue);
  end;

var
  i, iCount: Integer;
  xCandel1, xCandel2: TCandel;
begin
  /// Польное совпадение
  Result := True;

  if AStructure1.Sources.Count <> AStructure2.Sources.Count then
  begin
    Result := False;
    Exit;
  end;

  iCount := AStructure1.Sources.Count;
  if iCount > 0 then
    for i := 0 to iCount - 1 do
    begin
      xCandel1 := AStructure1.Sources[i];
      xCandel2 := AStructure2.Sources[i];
      if not _IsCandel(xCandel1,xCandel2,20) then
      begin
        Result := False;
        Break;
      end;
    end;
end;

{ TStructure }

constructor TStructure.Create;
begin
  FSources := TCandelList.Create;
end;

constructor TStructure.CreateConvert(AStructure: TStructure);

  procedure _ToMaxMinPrice(const ACandels: TCandelList; var AMaxPrice, AMinPrice: Double);
  begin
    if ACandels.Count < FSourceCount then
      raise Exception.Create('Error Message: Количество свечей не может быть меньше SourceCount');

    AMaxPrice := 0;
    AMinPrice := 0;
    if ACandels.Count > 0 then
    begin
      AMaxPrice := ACandels[0].High;
      AMinPrice := ACandels[0].Low;
      for var i := 0 to FSourceCount - 1 do
      begin
        var xCandel := ACandels[i];
        if xCandel.High > AMaxPrice then
          AMaxPrice := xCandel.High;
        if xCandel.Low < AMinPrice then
          AMinPrice := xCandel.Low;
      end;
    end;
  end;

  procedure _ToMaxMinVol(const ACandels: TCandelList; var AMaxVol, AMinVol: Double);
  begin
    if ACandels.Count < FSourceCount then
      raise Exception.Create('Error Message: Количество свечей не может быть меньше SourceCount');

    AMaxVol := 0;
    AMinVol := 0;
    if ACandels.Count > 0 then
    begin
      AMaxVol := ACandels[0].Vol;
      AMinVol := ACandels[0].Vol;
      for var i := 0 to FSourceCount - 1 do
      begin
        var xCandel := ACandels[i];
        if xCandel.Vol > AMaxVol then
          AMaxVol := xCandel.Vol;
        if xCandel.Vol < AMinVol then
          AMinVol := xCandel.Vol;
      end;
    end;
  end;

var
  xCandelConvert: TCandel;
  xMaxPrice, xMinPrice: Double;
  xMaxVol, xMinVol: Double;
begin
  Self.Create;

  Self.SourceCount := AStructure.SourceCount;
  Self.VectorCount := AStructure.VectorCount;

  _ToMaxMinPrice(AStructure.Sources,xMaxPrice,xMinPrice);
  _ToMaxMinVol(AStructure.Sources,xMaxVol,xMinVol);


  Self.Sources.Clear;
  for var xCandel in AStructure.Sources do
  begin
    xCandelConvert.Create(xCandel,xMaxPrice,xMinPrice,xMaxVol,xMinVol);
    Self.Sources.Add(xCandelConvert);
  end;
end;

destructor TStructure.Destroy;
begin
  FreeAndNil(FSources);
  inherited;
end;

{ TGenerationStructures }

constructor TGenerationStructures.Create;
begin
  inherited Create;
  FSourceCount := 0;
  FVectorCount := 0;
end;

procedure TGenerationStructures.AddCandel(const ACandel: TCandel);
var
  xStructure: TStructure;
begin
  if Count > 0 then
  begin
    for var i := Count - 1 downto 0 do
    begin
      xStructure := Self.Items[i];
      xStructure.Sources.Add(ACandel);
      if xStructure.Sources.Count >= FCapacityCandels then
      begin
        DoStructurePacked(xStructure);
        Self.Delete(i);
      end
      else if xStructure.Sources.Count = 1 then
        DoStructurePacked(xStructure);
    end;
  end;

  xStructure := TStructure.Create;
  xStructure.Sources.Add(ACandel);
  xStructure.SourceCount := FSourceCount;
  xStructure.VectorCount := FVectorCount;
  Self.Add(xStructure);
end;

procedure TGenerationStructures.DoStructurePacked(const AStructure: TStructure);
begin
  if Assigned(FEventStructurePacked) then
    FEventStructurePacked(AStructure);
end;

procedure TGenerationStructures.SetSourceCount(const Value: Integer);
begin
  FSourceCount := Value;
  FCapacityCandels := FSourceCount + FVectorCount;
end;

procedure TGenerationStructures.SetVectorCount(const Value: Integer);
begin
  FVectorCount := Value;
  FCapacityCandels := FSourceCount + FVectorCount;
end;

{ TPaternStructures }

function TPaternStructures.AddStructure(const AStructure: TStructure): Integer;

  function _IndexOfStructure(const AStructure: TStructure): Integer;
  var
    xStructure: TStructure;
  begin
    Result := -1;
    for var i := 0 to Self.Count - 1 do
    begin
      xStructure := Self.Items[i];
      if SameStructureFull(xStructure,AStructure) then
      begin
        Result := i;
        Break;
      end;
    end;
  end;

begin
  Result := -1;
  if (Self.Count = 0) or (_IndexOfStructure(AStructure) < 0) then
    Result := Self.Add(AStructure);
end;

{ TStructureAPI }

class function TStructureAPI.GetSimpleMovingAverages(AStructure: TStructure): Double;
var
  i, iCount: Integer;
begin
  iCount := AStructure.SourceCount;
  if iCount > 0 then
  begin
    var xSum := 0.0;
    for i := 0 to iCount - 1 do
      xSum := xSum + AStructure.Sources[i].Close;
    Result := xSum/iCount;
  end
  else
    raise Exception.Create('Error Message: Не возможно рассчитать простую средние скользящие');
end;

class function TStructureAPI.IsSimpleMovingAverages(AStructure: TStructure; VectorCount: Integer): Boolean;
var
  xValueMA: Double;
  xCandelLast, xCandel: TCandel;
  iCount: Integer;
begin
  iCount := AStructure.SourceCount;
  if iCount > 0 then
  begin
    xCandelLast := AStructure.Sources[iCount - 1];
    xCandel     := AStructure.Sources[iCount + (VectorCount - 1)];
    xValueMA := TStructureAPI.GetSimpleMovingAverages(AStructure);
    if xCandelLast.Close > xValueMA then
      Result := xCandelLast.Close < xCandel.Close
    else
      Result := xCandelLast.Close > xCandel.Close;
  end
  else
    raise Exception.Create('Error Message: Не возможно рассчитать простую средние скользящие');
end;

end.
