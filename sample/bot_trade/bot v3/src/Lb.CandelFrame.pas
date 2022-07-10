unit Lb.CandelFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Layouts, FMX.Objects,
  Lb.Candel.SysUtils;

type
  TCandelFrame = class(TFrame)
    LayoutTop: TLayout;
    LayoutBottom: TLayout;
    LayoutBody: TLayout;
    LayoutHigh: TLayout;
    LayoutLow: TLayout;
    RectangleBody: TRectangle;
    LayoutClient: TLayout;
    GridTop: TGridPanelLayout;
    Layout1: TLayout;
    Line1: TLine;
    GridBottom: TGridPanelLayout;
    Layout2: TLayout;
    Line2: TLine;
    RectangleFon: TRectangle;
  public type
    TPricePosition = record
      TopPrice: Single;
      High: Single;
      Low: Single;
      BottomPrice: Single;
    end;
    TTypeCandel = (tcNull,tcGreen,tcRed);
  private
    FPositionCandel: TPricePosition;
    FMaxPrice, FMinPrice: Double;
    FCandel: TCandel;
    procedure SetColorCandel(const ATypeCandel: TTypeCandel);
  protected
    function GetTopPrice: Double;
    function GetBottomPrice: Double;
    function GetHeightToPrice(const APrice: Double): Single;
    class function GetNameCandelFrame: String; static;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShow(const ACandel: TCandel; AMaxPrice, AMinPrice: Double; const AIsComing: Boolean = false);
  end;

implementation

{$R *.fmx}

var
  localIndex: Integer = 0;

{ TCandelFrame }

constructor TCandelFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  Self.Name := GetNameCandelFrame;
end;

destructor TCandelFrame.Destroy;
begin

  inherited;
end;

class function TCandelFrame.GetNameCandelFrame: String;
begin
  Inc(localIndex);
  Result := 'candel_frame_' + localIndex.ToString;
end;

function TCandelFrame.GetHeightToPrice(const APrice: Double): Single;
begin
  Result := LayoutClient.Height * (FMaxPrice - APrice)/(FMaxPrice - FMinPrice);
end;

function TCandelFrame.GetTopPrice: Double;
var
  xValue: Double;
begin
  xValue := FCandel.Close;
  if FCandel.Open > FCandel.Close then
    xValue := FCandel.Open;
  Result := xValue
end;

function TCandelFrame.GetBottomPrice: Double;
var
  xValue: Double;
begin
  xValue := FCandel.Close;
  if FCandel.Open < FCandel.Close then
    xValue := FCandel.Open;
  Result := xValue
end;

procedure TCandelFrame.SetColorCandel(const ATypeCandel: TTypeCandel);
begin
  case ATypeCandel of
    tcGreen: begin
      Line1.Stroke.Color := TAlphaColorRec.Green;
      Line2.Stroke.Color := TAlphaColorRec.Green;
      RectangleBody.Stroke.Color := TAlphaColorRec.Green;
      RectangleBody.Fill.Color := TAlphaColorRec.Green;
    end;
    tcRed: begin
      Line1.Stroke.Color := TAlphaColorRec.Red;
      Line2.Stroke.Color := TAlphaColorRec.Red;
      RectangleBody.Stroke.Color := TAlphaColorRec.Red;
      RectangleBody.Fill.Color := TAlphaColorRec.Red;
    end;
  else
    Line1.Stroke.Color := TAlphaColorRec.Black;
    Line2.Stroke.Color := TAlphaColorRec.Black;
    RectangleBody.Stroke.Color := TAlphaColorRec.Black;
  end;
end;

procedure TCandelFrame.SetShow(const ACandel: TCandel; AMaxPrice, AMinPrice: Double; const AIsComing: Boolean);

  procedure _HieghtLayoutTop;
  begin
    LayoutTop.Height := FPositionCandel.High;
  end;

  procedure _HieghtLayoutBottom;
  begin
    LayoutBottom.Height := LayoutClient.Height - FPositionCandel.Low;
  end;

  procedure _HieghtLayoutHigh;
  begin
    //LayoutHigh.Visible := FPositionCandel.High <> FPositionCandel.TopPrice;
    //if LayoutHigh.Visible then
    LayoutHigh.Height := FPositionCandel.TopPrice - FPositionCandel.High - 0.5;
  end;

  procedure _HieghtLayoutLow;
  begin
    //LayoutLow.Visible := FPositionCandel.Low <> FPositionCandel.BottomPrice;
    //if LayoutLow.Visible then
    LayoutLow.Height := FPositionCandel.Low - FPositionCandel.BottomPrice - 0.5;
  end;

begin
  FMaxPrice := AMaxPrice;
  FMinPrice := AMinPrice;
  FCandel   := ACandel;
  if (FMaxPrice > 0) and (FMinPrice > 0) and (FMaxPrice > FMinPrice) then
  begin
    FPositionCandel.TopPrice := GetHeightToPrice(GetTopPrice);
    FPositionCandel.High := GetHeightToPrice(FCandel.High);
    FPositionCandel.Low  := GetHeightToPrice(FCandel.Low);
    FPositionCandel.BottomPrice := GetHeightToPrice(GetBottomPrice);

    _HieghtLayoutTop;
    _HieghtLayoutBottom;
    _HieghtLayoutHigh;
    _HieghtLayoutLow;



    if FCandel.Open < FCandel.Close then
      SetColorCandel(TTypeCandel.tcGreen)
    else if FCandel.Open > FCandel.Close then
      SetColorCandel(TTypeCandel.tcRed)
    else
      SetColorCandel(TTypeCandel.tcNull);

    if AIsComing then
      RectangleFon.Fill.Color := TAlphaColorRec.Palegreen
    else
      RectangleFon.Fill.Color := TAlphaColorRec.Null;
  end;
end;

end.
