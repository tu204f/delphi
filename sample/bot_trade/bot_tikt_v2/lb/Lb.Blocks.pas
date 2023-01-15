unit Lb.Blocks;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections;

type
  TTypeBlock = (
    tbNormal, // Нейтральное состояние
    tbUp,     // Движение блока верх
    tbDown    // Движение блока вниз
  );

  ///<summary>Блок</summary>
  TBlock = class(TObject)
  public
    // Предельное значение цены
    PriceMax: Double;
    PriceMin: Double;

    // Текущие значение положение цена
    CurrentMax: Double;
    CurrentMin: Double;

    Vol: Integer;

    TypeBlock: TTypeBlock;

    constructor Create; virtual;
    destructor Destroy; override;
    ///<summary>Принадлежность блоку, включительно</summary>
    function IsAff(const APrice: Double): Boolean;
  end;
  TBlockList = TObjectList<TBlock>;

  TOnEndBlock = procedure(Sender: TObject; APrice: Double; ATypeBlock: TTypeBlock) of object;

  ///<summary>Колонка блоков</summary>
  TBlocks = class(TObject)
  private
    FMinStep: Double;     // Шаг цены
    FPridelMax: Integer;  // Максимальный придел
    FPridelMin: Integer;  // Минимальный придел
    FCurrentBlock: TBlock;
    FItems: TBlockList;
  protected
    FOnEndBlock: TOnEndBlock;
    FOnBeginBlock: TNotifyEvent;
    procedure DoEndBlock(APrice: Double; ATypeBlock: TTypeBlock);
    procedure DoBeginBlock;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure Clear;
    procedure UpData(const APrice: Double; const AVol: Integer);
    ///<summary>Шаг цены</summary>
    property MinStep: Double read FMinStep write FMinStep;
    ///<summary>Максимальный придел</summary>
    property PridelMax: Integer read FPridelMax write FPridelMin;
    ///<summary>Минимальный придел</summary>
    property PridelMin: Integer read FPridelMin write FPridelMin;
    property Items: TBlockList read FItems;
    property OnBeginBlock: TNotifyEvent write FOnBeginBlock;
    property OnEndBlock: TOnEndBlock write FOnEndBlock;
  end;



implementation

{ TBlock }

constructor TBlock.Create;
begin
  TypeBlock := TTypeBlock.tbNormal;
  PriceMax := 0;
  PriceMin := 0;
end;

destructor TBlock.Destroy;
begin

  inherited;
end;

function TBlock.IsAff(const APrice: Double): Boolean;
begin
  Result := (PriceMax >= APrice) and (PriceMin <= APrice);
end;

{ TBlocks }

constructor TBlocks.Create;
begin
  FMinStep := 1;
  FPridelMax := 100;
  FPridelMin := 100;
  FCurrentBlock := nil;
  FItems := TBlockList.Create;
end;

destructor TBlocks.Destroy;
begin
  FreeAndNil(FItems);
  inherited;
end;

procedure TBlocks.Clear;
begin
  FItems.Clear;
end;

procedure TBlocks.DoEndBlock(APrice: Double; ATypeBlock: TTypeBlock);
begin
  if Assigned(FOnEndBlock) then
    FOnEndBlock(Self,APrice,ATypeBlock);
end;

procedure TBlocks.DoBeginBlock;
begin
  if Assigned(FOnBeginBlock) then
    FOnBeginBlock(Self);
end;

procedure TBlocks.UpData(const APrice: Double; const AVol: Integer);

  function _GetCreateBlock(APrice: Double; AVol: Integer): TBlock;
  var
    xBlock: TBlock;
  begin
    xBlock := TBlock.Create;
    xBlock.Vol := AVol;
    xBlock.PriceMax := APrice + FPridelMax * FMinStep;
    xBlock.PriceMin := APrice - FPridelMin * FMinStep;
    xBlock.CurrentMax := APrice;
    xBlock.CurrentMin := APrice;
    FItems.Add(xBlock);
    Result := xBlock;
  end;

begin
  if Assigned(FCurrentBlock) then
  begin
    if FCurrentBlock.IsAff(APrice) then
    begin
      if FCurrentBlock.CurrentMax < APrice then
        FCurrentBlock.CurrentMax := APrice;
      if FCurrentBlock.CurrentMin > APrice then
        FCurrentBlock.CurrentMin := APrice;
      FCurrentBlock.Vol := FCurrentBlock.Vol + AVol
    end
    else
    begin
      // Вышли за пределы текущего блока
      if FCurrentBlock.PriceMax <= APrice then
      begin
        DoEndBlock(APrice,TTypeBlock.tbUp);
        FCurrentBlock.TypeBlock := TTypeBlock.tbUp;
      end
      else if FCurrentBlock.PriceMin >= APrice then
      begin
        DoEndBlock(APrice,TTypeBlock.tbDown);
        FCurrentBlock.TypeBlock := TTypeBlock.tbDown;
      end;
      FCurrentBlock := _GetCreateBlock(APrice, AVol);
      DoBeginBlock;
    end;
  end
  else
  begin
    FCurrentBlock := _GetCreateBlock(APrice, AVol);
    DoBeginBlock;
  end;
end;

end.
