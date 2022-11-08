unit UnitBarFrame;

interface

{$DEFINE PRICE_BAR}
{$DEFINE PRICE_CANDEL}

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  System.Generics.Collections,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  FMX.Layouts,
  FMX.Objects,
  Lb.SysUtils.Candel;

type
  ///<summary>Одна свеча</summary>
  TBarFrame = class(TFrame)
    Layout: TLayout;
    Rectangle1: TRectangle;
    LayoutCandel: TLayout;
    LineCandelHL: TLine;
    RectangleBody: TRectangle;
    LineFlet: TLine;
  private
    FMaxValue: Double;
    FMinValue: Double;
    FCandel: TCandel;
    function GetColorBody: TAlphaColor;
  protected
    procedure Loaded; override;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowCandel;
    procedure SetCandel(const AMaxValue, AMinValue: Double; const ACandel: TCandel);
  end;

  ///<summary>Список свечей</summary>
  TBarFrameList = TObjectList<TBarFrame>;

implementation

{$R *.fmx}

function GetMaxValue(const AValue1, AValue2: Double): Double;
begin
  if AValue1 > AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

function GetMinValue(const AValue1, AValue2: Double): Double;
begin
  if AValue1 < AValue2 then
    Result := AValue1
  else
    Result := AValue2;
end;

{ TBarFrame }

constructor TBarFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TBarFrame.Destroy;
begin

  inherited;
end;

procedure TBarFrame.Loaded;
begin
  inherited Loaded;
  Rectangle1.Align := TAlignLayout.Client;
end;

function TBarFrame.GetColorBody: TAlphaColor;
begin
  if FCandel.Open < FCandel.Close then
    Result := TAlphaColorRec.Green
  else if FCandel.Open > FCandel.Close then
    Result := TAlphaColorRec.Red
  else
    Result := TAlphaColorRec.Black
end;

procedure TBarFrame.SetCandel(const AMaxValue, AMinValue: Double;
  const ACandel: TCandel);
begin
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
  FCandel   := ACandel;
end;


procedure TBarFrame.SetShowCandel;

  function _GetPositionY(const APrice, AMaxValue, AMinValue: Double; const AHieght: Single): Single;
  begin
    Result := 0;
    if (AMaxValue <> AMinValue) then
      Result := AHieght * ((AMaxValue - APrice)/(AMaxValue - AMinValue));
  end;

  function _GetColorFillBody: TAlphaColor;
  begin
    if FCandel.Open < FCandel.Close then
      Result := TAlphaColorRec.Lightgreen
    else if FCandel.Open > FCandel.Close then
      Result := TAlphaColorRec.Lightpink
    else
      Result := TAlphaColorRec.Gray
  end;

  function _GetPrice(const AValue1, AValue2: Double; const ActiveMaxValue: Boolean): Double;
  begin
    if ActiveMaxValue then
    begin
      if AValue1 > AValue2 then
        Result := AValue1
      else
        Result := AValue2;
    end
    else
    begin
      if AValue1 < AValue2 then
        Result := AValue1
      else
        Result := AValue2;
    end;
  end;

  procedure _SetShowCandelImage;
  var
    xTopPrice, xBottomPrice: Double;
    xTopBody, xHigh, xLow, xBottomBody: Single;
    X, Y, xWidth, xHeight, xDeltaWidth: Single;
  begin

    xTopPrice := _GetPrice(FCandel.Open, FCandel.Close, True);
    xBottomPrice := _GetPrice(FCandel.Open, FCandel.Close, False);

    xTopBody    := _GetPositionY(xTopPrice,     FMaxValue, FMinValue, LayoutCandel.Height);
    xHigh       := _GetPositionY(FCandel.High,  FMaxValue, FMinValue, LayoutCandel.Height);
    xLow        := _GetPositionY(FCandel.Low,   FMaxValue, FMinValue, LayoutCandel.Height);
    xBottomBody := _GetPositionY(xBottomPrice,  FMaxValue, FMinValue, LayoutCandel.Height);

    case FCandel.Status of
      tcCurrent: Rectangle1.Fill.Color := TAlphaColorRec.Yellow;
      tcSource : Rectangle1.Fill.Color := TAlphaColorRec.Null;
      tcFuture : Rectangle1.Fill.Color := TAlphaColorRec.Lime;
    end;

    if xTopBody >= xBottomBody  then
    begin
      LineFlet.Visible := True;
      RectangleBody.Visible := False;
    end
    else
    begin
      LineFlet.Visible := False;
      RectangleBody.Visible := True;
    end;


    xDeltaWidth := LayoutCandel.Width/2; // - (LineBody.Stroke.Thickness/2);

    // Тело бара
    X := xDeltaWidth;
    Y := xHigh;
    xHeight := xLow - xHigh;
    xWidth := LineCandelHL.Width;
    LineCandelHL.SetBounds(X, Y, xWidth, xHeight);

    if RectangleBody.Visible then
    begin
      X := LineCandelHL.Stroke.Thickness;
      Y := xTopBody;
      xHeight := xBottomBody - xTopBody;
      xWidth  := LayoutCandel.Width - 2 * LineCandelHL.Stroke.Thickness;
      RectangleBody.SetBounds(X,Y,xWidth,xHeight);

      LineCandelHL.Stroke.Color := GetColorBody;
      RectangleBody.Stroke.Color := GetColorBody;
      RectangleBody.Fill.Color := _GetColorFillBody;
    end
    else
    begin
      X := LineCandelHL.Stroke.Thickness;
      Y := xTopBody;
      xHeight := LineCandelHL.Height;
      xWidth  := LayoutCandel.Width - 2 * LineCandelHL.Stroke.Thickness;
      LineFlet.SetBounds(X,Y,xWidth,xHeight);

      LineCandelHL.Stroke.Color := GetColorBody;
      LineFlet.Stroke.Color := GetColorBody;
    end;

  end;

begin
  _SetShowCandelImage;
end;

end.
