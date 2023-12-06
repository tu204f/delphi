unit UnitChartCandelsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  Lb.SysUtils.Candel, FMX.Layouts,   System.Generics.Collections, FMX.Objects;

type
  ///<summary>Список</summary>
  TArrowPathList = TObjectList<TPath>;

  ///<summary>Отображение графика</summary>
  TChartCandelsFrame = class(TFrame)
    LayoutCandels: TLayout;
    RectangleCandelBar: TRectangle;
    LayoutPriceClose: TLayout;
    TextPriceClose: TText;
    PathLine: TPath;

  public type


    ///<summary>Ценовая значение</summary>
    TCandelBar = class(TLayout)
    public const
      LINE_WIDTH = 2;
    private
      Rectangle: TRectangle;
      LineBar: TLine;
      LineOpen: TLine;
      LineClose: TLine;
    private
      FCandel: TCandel;
      FMaxPrice: Double;
      FMinPrice: Double;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure Build;
      property Candel  : TCandel read FCandel write FCandel;
      property MaxPrice: Double  write FMaxPrice;
      property MinPrice: Double  write FMinPrice;
    end;
    TCandelBarList = TObjectList<TCandelBar>;

  private
    FCandels: TCandels;
    FCandelBars: TCandelBarList;
  protected
    procedure PositionLayout;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    ///<summary>Свечи которые используем для построения</summary>
    property Candels: TCandels read FCandels;
    ///<summary>Построить график</summary>
    procedure BuildChart;
  end;

implementation

{$R *.fmx}

function GetY(const AHeight: Single; const APrice, AMaxPrice, AMinPrice: Double): Single;
begin
  Result := 0;
  if AHeight <= 0 then
    Exit;
  var xDeltaPrice := AMaxPrice - AMinPrice;
  if xDeltaPrice > 0 then
  begin
    var xDeltaPoint := xDeltaPrice/AHeight;
    Result := (AMaxPrice - APrice)/xDeltaPoint;
  end
  else
  begin
    var xS := 'MaxPrice: ' + FloatToStr(AMaxPrice) + '; MinPrice: ' + FloatToStr(AMinPrice);
    raise Exception.Create('Error Message: Не верное значение ' + xS);
  end;
end;

const
  ARROW_SVG_DOWN =
    'M164.29231262207,6524.36572265625 L164.29231262207,6524.36572265625 ' +
    'C163.902557373047,6524.7705078125 163.902557373047,6525.42626953125 ' +
    '164.29231262207,6525.830078125 L172.555877685547,6534.392578125 ' +
    'C173.336364746094,6535.20263671875 174.602523803711,6535.20263671875 ' +
    '175.383010864258,6534.392578125 L183.707534790039,6525.76806640625 ' +
    'C184.093292236328,6525.3671875 184.098281860352,6524.7197265625 ' +
    '183.717529296875,6524.31396484375 C183.328796386719,6523.89990234375 ' +
    '182.688217163086,6523.89453125 182.293472290039,6524.302734375 ' +
    'L174.676483154297,6532.1962890625 C174.285736083984,6532.60107421875 ' +
    '173.65315246582,6532.60107421875 173.262405395508,6532.1962890625 ' +
    'L165.705383300781,6524.36572265625 C165.315628051758,6523.9609375 ' +
    '164.683044433594,6523.9609375 164.29231262207,6524.36572265625';

  ARROW_SVG_UP =
    'M164.29231262207,6524.36572265625 L164.29231262207,6524.36572265625 ' +
    'C163.902557373047,6524.7705078125 163.902557373047,6525.42626953125 ' +
    '164.29231262207,6525.830078125 L172.555877685547,6534.392578125 ' +
    'C173.336364746094,6535.20263671875 174.602523803711,6535.20263671875 ' +
    '175.383010864258,6534.392578125 L183.707534790039,6525.76806640625 ' +
    'C184.093292236328,6525.3671875 184.098281860352,6524.7197265625 ' +
    '183.717529296875,6524.31396484375 C183.328796386719,6523.89990234375 ' +
    '182.688217163086,6523.89453125 182.293472290039,6524.302734375 ' +
    'L174.676483154297,6532.1962890625 C174.285736083984,6532.60107421875 ' +
    '173.65315246582,6532.60107421875 173.262405395508,6532.1962890625 ' +
    'L165.705383300781,6524.36572265625 C165.315628051758,6523.9609375 ' +
    '164.683044433594,6523.9609375 164.29231262207,6524.36572265625';

{ TChartCandelsFrame.TCandelBar }

constructor TChartCandelsFrame.TCandelBar.Create(AOwner: TComponent);

  procedure _LineProperty(const ALine: TLine);
  begin
    with ALine do
    begin
      Width := LINE_WIDTH;
      Stroke.Thickness := LINE_WIDTH;
      Parent := Self;
    end;
  end;

begin
  inherited;

  Rectangle := TRectangle.Create(nil);
  Rectangle.Parent := Self;
  Rectangle.Align  := TAlignLayout.Client;
  Rectangle.Stroke.Color := Rectangle.Fill.Color;

  LineBar := TLine.Create(nil);
  _LineProperty(LineBar);
  LineBar.LineType := TLineType.Left;

  LineOpen := TLine.Create(nil);
  _LineProperty(LineOpen);
  LineOpen.LineType := TLineType.Top;

  LineClose := TLine.Create(nil);
  _LineProperty(LineClose);
  LineClose.LineType := TLineType.Top;
end;

destructor TChartCandelsFrame.TCandelBar.Destroy;
begin
  FreeAndNil(LineOpen);
  FreeAndNil(LineClose);
  FreeAndNil(LineBar);
  FreeAndNil(Rectangle);
  inherited;
end;

procedure TChartCandelsFrame.TCandelBar.Build;

  procedure _LineBar;
  var
    X, Y1, Y2, xW, xH: Single;
  begin
    X  := Self.Width/2;
    Y1 := GetY(Self.Height,FCandel.High,FMaxPrice,FMinPrice);
    Y2 := GetY(Self.Height,FCandel.Low, FMaxPrice,FMinPrice);
    xH := Y2 - Y1;
    xW := LINE_WIDTH;
    LineBar.SetBounds(X,Y1,xW,xH);
  end;

  procedure _LineOpen;
  var
    X, Y, xW, xH: Single;
  begin
    X  := 0;
    Y  := GetY(Self.Height,FCandel.Open,FMaxPrice,FMinPrice);
    xH := LINE_WIDTH;
    xW := Self.Width/2 + LINE_WIDTH;
    LineOpen.SetBounds(X,Y,xW,xH);
  end;

  procedure _LineClose;
  var
    X, Y, xW, xH: Single;
  begin
    X  := Self.Width/2;
    Y  := GetY(Self.Height,FCandel.Close,FMaxPrice,FMinPrice);
    xH := LINE_WIDTH;
    xW := Self.Width/2;
    LineClose.SetBounds(X,Y,xW,xH);
  end;

  procedure _LineColor;
  begin
    if FCandel.Open < FCandel.Close then
    begin
      LineBar.Stroke.Color := TAlphaColorRec.Green;
      LineOpen.Stroke.Color := TAlphaColorRec.Green;
      LineClose.Stroke.Color := TAlphaColorRec.Green;
    end
    else
    begin
      LineBar.Stroke.Color := TAlphaColorRec.Red;
      LineOpen.Stroke.Color := TAlphaColorRec.Red;
      LineClose.Stroke.Color := TAlphaColorRec.Red;
    end;
  end;

begin
  LineBar.Visible   := not FCandel.IsEmptyPrice;

  LineOpen.Visible  := not FCandel.IsEmptyPrice;
  LineClose.Visible := not FCandel.IsEmptyPrice;

  if not FCandel.IsEmptyPrice then
  begin
    _LineBar;
    _LineOpen;
    _LineClose;
    _LineColor;
  end;
end;

{ TChartCandelFrame }

constructor TChartCandelsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FCandels    := TCandels.Create;
  FCandelBars := TCandelBarList.Create;
end;


destructor TChartCandelsFrame.Destroy;
begin
  FreeAndNil(FCandelBars);
  FreeAndNil(FCandels);
  inherited;
end;

procedure TChartCandelsFrame.PositionLayout;

  procedure _SetCreateBar;
  var
    xCandelBar: TCandelBar;
  begin
    if FCandels.Count <> FCandelBars.Count then
    begin
      FCandelBars.Clear;
      for var i := 0 to FCandels.Count - 1 do
      begin
        // Ценовой бар
        xCandelBar := TCandelBar.Create(nil);
        xCandelBar.Parent := LayoutCandels;
        FCandelBars.Add(xCandelBar);
      end;
    end;
  end;

  procedure _SetPositionBar;
  var
    xCandelBar: TCandelBar;
    X, Y, xDeltaWidth: Single;
  begin
    if FCandelBars.Count > 0 then
    begin
      xDeltaWidth := LayoutCandels.Width / FCandels.Count;

      for var i := 0 to FCandelBars.Count - 1 do
      begin

        // Ценовой бар
        xCandelBar := FCandelBars[i];
        X := i * xDeltaWidth;
        Y := 0;
        xCandelBar.SetBounds(
          X,
          Y,
          xDeltaWidth,
          LayoutCandels.Height
        );

      end;
    end;
  end;

  procedure _SetPositionValueBar;
  var
    xCandelBar: TCandelBar;
    xCandel: TCandel;
  begin
    if FCandels.Count > 0 then
      for var i := 0 to FCandels.Count - 1 do
      begin

        xCandelBar := FCandelBars[i];
        xCandel    := FCandels[i];

        xCandelBar.MinPrice := FCandels.MinPrice;
        xCandelBar.MaxPrice := FCandels.MaxPrice;
        xCandelBar.Candel   := xCandel;
        xCandelBar.Build;

        // Показать цену последную цену
        if i = (FCandels.Count - 1) then
        begin
          TextPriceClose.SetBounds(
            5,
            xCandelBar.LineClose.Position.Y - 12.5,
            135,
            25
          );
          TextPriceClose.Text := (Trunc(1000 * xCandelBar.Candel.Close) / 1000).ToString;
        end;

      end;
  end;

begin
  _SetCreateBar;
  _SetPositionBar;
  _SetPositionValueBar;
end;

procedure TChartCandelsFrame.BuildChart;
begin

  FCandels.LimitValues;
  PositionLayout;
end;


end.
