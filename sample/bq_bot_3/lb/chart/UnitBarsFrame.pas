unit UnitBarsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects,
  System.Generics.Collections, FMX.Memo.Types, FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Memo,
  UnitBarFrame;

type
  ///<summary>
  /// Источник данных
  ///</summary>
  TSources = class(TObject)
  public type
    TCandel = record
      Open: Double;
      High: Double;
      Low: Double;
      Close: Double;
    end;
    TCandelList = TList<TCandel>;
  private
    FHeight: Single;
    FPriceCandels: TCandelList;
    FCoordCandels: TCandelList;
  public
    constructor Create; virtual;
    destructor Destroy; override;
    procedure SetUpDateCoordinates;
    ///<summary>Определение координаты цены, относительно высоты</summary>
    property Height: Single read FHeight write FHeight;
    ///<summary>Цена</summary>
    property PriceCandels: TCandelList read FPriceCandels;
    ///<summary>Координаты цена по высоте</summary>
    property CoordCandels: TCandelList read FCoordCandels;
  end;

  TBarFrameList = TObjectList<TBarFrame>;

  TBarsFrame = class(TFrame)
    Rectangle: TRectangle;
    Timer: TTimer;
    procedure TimerTimer(Sender: TObject);
  private
    FBarFrames: TBarFrameList;
    FSources: TSources;
    function GetIsActive: Boolean;
    procedure SetIsActive(const Value: Boolean);
  protected
    procedure SetUpDateBars;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    procedure AddCandel(const AOpen, AHigh, ALow, AClose: Double);
    property IsActive: Boolean read GetIsActive write SetIsActive;
  end;

implementation

{$R *.fmx}

const
  WIDTH_FRAME = 10;

{ TSources }

constructor TSources.Create;
begin
  FPriceCandels := TCandelList.Create;
  FCoordCandels := TCandelList.Create;

end;

destructor TSources.Destroy;
begin
  FreeAndNil(FCoordCandels);
  FreeAndNil(FPriceCandels);
  inherited;
end;

procedure TSources.SetUpDateCoordinates;

  procedure _MaxAndMinPrice(var AMaxPrice, AMinPrice: Double);
  var
    xCandel: TSources.TCandel;
  begin
    AMaxPrice := 0;
    AMinPrice := 0;
    if FPriceCandels.Count > 0 then
    begin
      xCandel := FPriceCandels[0];
      AMaxPrice := xCandel.High;
      AMinPrice := xCandel.Low;
      for var i := 1 to FPriceCandels.Count - 1 do
      begin
        xCandel := FPriceCandels[i];
        if xCandel.High > AMaxPrice then
          AMaxPrice := xCandel.High;
        if xCandel.Low < AMinPrice then
          AMinPrice := xCandel.Low;
      end;
    end;
  end;

  function _ToY(const APrice, AMaxPrice, AMinPrice: Double): Single;
  begin
    Result := FHeight * (AMaxPrice - APrice)/(AMaxPrice - AMinPrice);
  end;

var
  xMaxPrice, xMinPrice: Double;
  xPriceCandel, xCoordCandel: TSources.TCandel;
begin
  FCoordCandels.Clear;
  _MaxAndMinPrice(xMaxPrice, xMinPrice);
  if (xMaxPrice > xMinPrice) and (xMaxPrice > 0) and (xMinPrice > 0) then
  begin
    for xPriceCandel in FPriceCandels do
    begin
      xCoordCandel.Open  := _ToY(xPriceCandel.Open ,xMaxPrice,xMinPrice);
      xCoordCandel.High  := _ToY(xPriceCandel.High ,xMaxPrice,xMinPrice);
      xCoordCandel.Low   := _ToY(xPriceCandel.Low  ,xMaxPrice,xMinPrice);
      xCoordCandel.Close := _ToY(xPriceCandel.Close,xMaxPrice,xMinPrice);
      FCoordCandels.Add(xCoordCandel);
    end;
  end;
end;

{ TBarsFrame }

constructor TBarsFrame.Create(AOwner: TComponent);
begin
  inherited;
  FBarFrames := TBarFrameList.Create;
  FSources := TSources.Create;
end;

destructor TBarsFrame.Destroy;
begin
  FreeAndNil(FSources);
  FreeAndNil(FBarFrames);
  inherited;
end;

function TBarsFrame.GetIsActive: Boolean;
begin
  Result := Timer.Enabled;
end;

procedure TBarsFrame.SetIsActive(const Value: Boolean);
begin
  Timer.Enabled := Value;
end;

procedure TBarsFrame.TimerTimer(Sender: TObject);
begin
  try
    SetUpDateBars;
  except
    Self.IsActive := False;
    raise Exception.Create('Error Message: Ошибка обновление');
  end;
end;

procedure TBarsFrame.SetUpDateBars;

  procedure _SetCreateBarFrame;
  var
    xDeltaWidth: Single;
    i, iCount: Integer;
    xBarFrame: TBarFrame;
    xX, xY, xWidht, xHeight: Single;
    xCandel: TSources.TCandel;
  begin
    FBarFrames.Clear;
    iCount := FSources.CoordCandels.Count;
    if iCount > 0 then
    begin
      xDeltaWidth := Rectangle.Width / iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandel := FSources.CoordCandels[i];

        xBarFrame := TBarFrame.Create(nil);
        xBarFrame.Parent := Rectangle;
        xBarFrame.ID := i;

        xX := Rectangle.Width - xDeltaWidth * (i + 1);
        xY := 0;
        xWidht := xDeltaWidth;
        xHeight := Rectangle.Height;

        xBarFrame.SetBounds(xX, xY, xWidht, xHeight);
        xBarFrame.SetBoundsCandel(
          xCandel.Open,
          xCandel.High,
          xCandel.Low,
          xCandel.Close
        );

        FBarFrames.Add(xBarFrame);
      end;
    end;
  end;

  procedure _SetUpDatePositionBarFrame;
  var
    xDeltaWidth: Single;
    i, iCount: Integer;
    xBarFrame: TBarFrame;
    xX, xY, xWidht, xHeight: Single;
    xCandel: TSources.TCandel;
  begin
    iCount := FSources.CoordCandels.Count;
    if iCount > 0 then
    begin
      xDeltaWidth := Rectangle.Width / iCount;
      for i := 0 to iCount - 1 do
      begin
        xCandel := FSources.CoordCandels[i];

        xBarFrame := FBarFrames[i];

        xX := Rectangle.Width - xDeltaWidth * (i + 1);
        xY := 0;
        xWidht := xDeltaWidth;
        xHeight := Rectangle.Height;

        xBarFrame.SetBounds(xX, xY, xWidht, xHeight);
        xBarFrame.SetBoundsCandel(
          xCandel.Open,
          xCandel.High,
          xCandel.Low,
          xCandel.Close
        );

      end;
    end;
  end;

  procedure _SetUpDatePriceBarFrame;
  var
    i, iCount: Integer;
    xBarFrame: TBarFrame;
    xCandel: TSources.TCandel;
  begin
    iCount := FSources.CoordCandels.Count;
    if iCount > 0 then
    begin
      for i := 0 to iCount - 1 do
      begin
        xCandel := FSources.CoordCandels[i];
        xBarFrame := FBarFrames[i];
        xBarFrame.SetBoundsCandel(xCandel.Open,xCandel.High,xCandel.Low,xCandel.Close);
      end;
    end;
  end;


begin
  FSources.Height := Rectangle.Height;
  FSources.SetUpDateCoordinates;
  if (FBarFrames.Count > 0) and (FBarFrames.Count = FSources.CoordCandels.Count) then
    _SetUpDatePositionBarFrame
  else
    _SetCreateBarFrame;
  _SetUpDatePriceBarFrame;
end;

procedure TBarsFrame.Clear;
begin
  FSources.PriceCandels.Clear;
end;

procedure TBarsFrame.AddCandel(const AOpen, AHigh, ALow, AClose: Double);
var
  xCandel: TSources.TCandel;
begin
  xCandel.Open := AOpen;
  xCandel.High := AHigh;
  xCandel.Low :=  ALow;
  xCandel.Close := AClose;
  FSources.PriceCandels.Add(xCandel);
end;

end.
