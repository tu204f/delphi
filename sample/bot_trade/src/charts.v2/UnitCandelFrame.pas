unit UnitCandelFrame;

interface

{$DEFINE IMG_CND}

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
  Lb.Candel.SysUtils;

type
  ///<summary>Одна свеча</summary>
  TCandelFrame = class(TFrame)
    Layout: TLayout;
    LayoutTop: TLayout;
    LayoutBottom: TLayout;
    LayoutCandel: TLayout;
    LayoutHigh: TLayout;
    LayoutLow: TLayout;
    RectangleBody: TRectangle;
    LineHigh: TLine;
    LineLow: TLine;
    Rectangle2: TRectangle;
    LineBody: TLine;
    LineOpen: TLine;
    LineClose: TLine;
    Rectangle1: TRectangle;
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
  TCandelFrameList = TObjectList<TCandelFrame>;

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

{ TCandelFrame }

constructor TCandelFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TCandelFrame.Destroy;
begin

  inherited;
end;

procedure TCandelFrame.Loaded;
begin
  inherited Loaded;
  {$IFDEF IMG_CND}
  Rectangle2.Visible := False;
  Rectangle1.Visible := True;
  Rectangle1.Align := TAlignLayout.Client;
  {$ELSE}
  LayoutImage.Visible := False;
  Rectangle2.Visible := True;
  Rectangle2.Align := TAlignLayout.Client;
  {$ENDIF}
end;

function TCandelFrame.GetColorBody: TAlphaColor;
begin
  if FCandel.Open < FCandel.Close then
    Result := TAlphaColorRec.Green
  else if FCandel.Open > FCandel.Close then
    Result := TAlphaColorRec.Red
  else
    Result := TAlphaColorRec.Black
end;

procedure TCandelFrame.SetCandel(const AMaxValue, AMinValue: Double;
  const ACandel: TCandel);
begin
  FMaxValue := AMaxValue;
  FMinValue := AMinValue;
  FCandel   := ACandel;
  {$IFDEF IMG_CND}

  case FCandel.Status of
    tcSource: Rectangle1.Fill.Color := TAlphaColorRec.Null;
    tcFuture: Rectangle1.Fill.Color := TAlphaColorRec.Lime;
  end;

  LineBody.Stroke.Color := GetColorBody;
  LineOpen.Stroke.Color := GetColorBody;
  LineClose.Stroke.Color := GetColorBody;

  {$ELSE}
  RectangleBody.Fill.Color := GetColorBody;
  {$ENDIF}
end;


procedure TCandelFrame.SetShowCandel;

  {$IFDEF IMG_CND}

  function GetPositionY(const APrice, AMaxValue, AMinValue: Double; const AHieght: Single): Single;
  begin
    Result := 0;
    if (AMaxValue > 0) and (AMinValue > 0) and (AMaxValue <> AMinValue) then
      Result := AHieght * ((AMaxValue - APrice)/(AMaxValue - AMinValue));
  end;

  procedure _SetShowCandelImage;
  var
    xOpen, xHigh, xLow, xClose: Single;
    X, Y, xWidth, xHeight, xDeltaWidth: Single;
  begin
    xOpen  := GetPositionY(FCandel.Open,  FMaxValue, FMinValue, Self.Height);
    xHigh  := GetPositionY(FCandel.High,  FMaxValue, FMinValue, Self.Height);
    xLow   := GetPositionY(FCandel.Low,   FMaxValue, FMinValue, Self.Height);
    xClose := GetPositionY(FCandel.Close, FMaxValue, FMinValue, Self.Height);


    xDeltaWidth := Rectangle1.Width/2; // - (LineBody.Stroke.Thickness/2);

    // Тело бара
    X := xDeltaWidth;
    Y := xHigh;
    xHeight := xLow - xHigh;
    xWidth := LineBody.Stroke.Thickness;
    LineBody.SetBounds(X, Y, xWidth, xHeight);

    // Открытие
    X := 0;
    Y := xOpen;
    xHeight := LineBody.Stroke.Thickness;
    xWidth := xDeltaWidth + LineBody.Stroke.Thickness;
    LineOpen.SetBounds(X, Y, xWidth, xHeight);


    // Открытие
    X := xDeltaWidth - LineBody.Stroke.Thickness/2;
    Y := xClose;
    xHeight := LineBody.Stroke.Thickness;
    xWidth := xDeltaWidth + LineBody.Stroke.Thickness;
    LineClose.SetBounds(X, Y, xWidth, xHeight);

  end;
  {$ELSE}
  function GetLayoutTopHeight(const ADelta: Double): Single;
  begin
    Result := (FMaxValue - FCandel.High)/ADelta;
  end;

  function GetLayoutBottomHeight(const ADelta: Double): Single;
  begin
    Result := (FCandel.Low - FMinValue)/ADelta;
  end;

  function GetLayoutHighHieght(const ADelta: Double): Single;
  var
    xValue: Double;
  begin
    xValue := GetMaxValue(FCandel.Open,FCandel.Close);
    Result := (FCandel.High - xValue)/ADelta;
  end;

  function GetLayoutLowHeight(const ADelta: Double): Single;
  var
    xValue: Double;
  begin
    xValue := GetMinValue(FCandel.Open,FCandel.Close);
    Result := (xValue - FCandel.Low)/ADelta;
  end;

  procedure _SetShowCandelRectangle;
  var
    xDelta: Double;
  begin
    xDelta := (FMaxValue - FMinValue)/Self.Height;

    if FCandel.Open <> FCandel.Close then
    begin
      LayoutTop.Height := GetLayoutTopHeight(xDelta);
      LayoutBottom.Height := GetLayoutBottomHeight(xDelta);
      LayoutHigh.Height := GetLayoutHighHieght(xDelta);
      LayoutLow.Height := GetLayoutLowHeight(xDelta);
    end
    else
    begin
      LayoutTop.Height := GetLayoutTopHeight(xDelta);
      LayoutBottom.Height := GetLayoutBottomHeight(xDelta);
      LayoutHigh.Height := GetLayoutHighHieght(xDelta) - 1;
      LayoutLow.Height := GetLayoutLowHeight(xDelta) - 1;
    end;
  end;
  {$ENDIF}

begin
  {$IFDEF IMG_CND}
  _SetShowCandelImage;
  {$ELSE}
  _SetShowCandelRectangle;
  {$ENDIF}
end;

end.
