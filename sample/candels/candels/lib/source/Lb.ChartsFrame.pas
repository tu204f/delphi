unit Lb.ChartsFrame;

interface

uses
  System.SysUtils, System.Types, System.UITypes, System.Classes, System.Variants, 
  FMX.Types, FMX.Graphics, FMX.Controls, FMX.Forms, FMX.Dialogs, FMX.StdCtrls,
  FMX.Objects, FMX.Layouts,
  Lb.Candel.SysUtils,
  Lb.Candel.Source,
  System.Generics.Collections;

type
  TChartsFrame = class(TFrame)
    Layout: TLayout;
    procedure LayoutResize(Sender: TObject);
  public type

    TCandel = class(TLayout)
    private
      FCandel: TCandel;
    protected
      LayoutHigh: TLayout;
      LayoutLow: TLayout;
      Body: TRectangle;
      LineHigh: TLine;
      LineLow: TLine;
      property Candel: TCandel read FCandel write FCandel;
    protected
      procedure Resize; override;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
    end;

    TCandelLayout = class(TLayout)
    protected
      Candel: TCandel;
      MaxValue: Double;
      MinValue: Double;
      SourceCandel: Lb.Candel.SysUtils.TCandel;
    public
      constructor Create(AOwner: TComponent); override;
      destructor Destroy; override;
      procedure SetPositionCandel;
    end;

    TCalbelLayoutList = TObjectList<TCandelLayout>;

  private
    FSource: TSourceCandel;
    FCalbelLayouts: TCalbelLayoutList;
  protected
    procedure ClearCandels;
    procedure SetShowCandels;
    procedure SetCreateCandels(const ACount: Integer); overload;
    procedure SetCreateCandels(const ABeginIndex, ACount: Integer); overload;
    function GetCreateCandelLayout(const AIndex: Integer): TChartsFrame.TCandelLayout;
    property CalbelLayouts: TCalbelLayoutList read FCalbelLayouts;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Source: TSourceCandel read FSource;
  public
    class function GetCreateFrame(const AParent: TFmxObject): TChartsFrame; static;
    class procedure SetFreeAndNil; static;
    ///<summary>Источние данных</summary>
    class function GetSource: TSourceCandel; static;
    ///<summary>Показать посление свечи (в количество  ACount)</summary>
    class procedure SetShowCharts(const ACount: Integer = 0); overload; static;
    ///<summary>Показать посление свечи (в количество  ACount), начиная с ABeginIndex</summary>
    class procedure SetShowCharts(const ABeginIndex, ACount: Integer); overload; static;
  end;

implementation

{$R *.fmx}

var
  localChartsFrame: TChartsFrame = nil;

{ TChartsFrame.TCandel }

constructor TChartsFrame.TCandel.Create(AOwner: TComponent);
begin
  inherited;

  LayoutHigh := TLayout.Create(Self);
  with LayoutHigh do
  begin
    Align := TAlignLayout.Top;
    Height := 20;
    Parent := Self;
  end;

  LayoutLow := TLayout.Create(Self);
  with LayoutLow do
  begin
    Align := TAlignLayout.Bottom;
    Height := 20;
    Parent := Self;
  end;

  Body := TRectangle.Create(Self);
  with Body do
  begin
    Align := TAlignLayout.Client;
    Parent := Self;
  end;

  LineHigh := TLine.Create(Self);
  with LineHigh do
  begin
    //Align := TAlignLayout.VertCenter;
    LineType := TLineType.Left;
    Parent := LayoutHigh;
    Stroke.Thickness := 1;
  end;

  LineLow := TLine.Create(Self);
  with LineLow do
  begin
    //Align := TAlignLayout.VertCenter;
    LineType := TLineType.Left;
    Parent := LayoutLow;
    Stroke.Thickness := 1;
  end;

end;

destructor TChartsFrame.TCandel.Destroy;
begin
  FreeAndNil(Body);
  FreeAndNil(LayoutLow);
  FreeAndNil(LayoutHigh);
  inherited;
end;

procedure TChartsFrame.TCandel.Resize;
var
  xX, xY, xWidth, xHeight: Single;
begin
  inherited Resize;

  if Assigned(LineHigh) then
  begin
    LineHigh.Align := TAlignLayout.None;
    xX := LayoutHigh.Width / 2 - 0.5;
    xY := 0;
    xWidth := 1;
    xHeight := LayoutHigh.Height;
    LineHigh.SetBounds(xX, xY, xWidth, xHeight);
  end;

  if Assigned(LineLow) then
  begin
    LineLow.Align := TAlignLayout.None;
    xX := LayoutLow.Width / 2 - 0.5;
    xY := 0;
    xWidth := 1;
    xHeight := LayoutLow.Height;
    LineLow.SetBounds(xX, xY, xWidth, xHeight);
  end;
end;

{ TChartsFrame.TCandelLayout }

constructor TChartsFrame.TCandelLayout.Create(AOwner: TComponent);
begin
  inherited;
  Candel := TCandel.Create(Self);
  with Candel do
  begin
    Align := TAlignLayout.None;
    Parent := Self;
    with Padding do
    begin
      Left := 0.5;
      Right := 0.5;
    end;
  end;
end;

destructor TChartsFrame.TCandelLayout.Destroy;
begin
  FreeAndNil(Candel);
  inherited;
end;

procedure TChartsFrame.TCandelLayout.SetPositionCandel;
var
  xTop, xBottom: Single;
  xTopOpen, xBottomClose: Single;
begin
  if (MaxValue > MinValue) then
  begin
    xTop := Self.Height * ((MaxValue - SourceCandel.High)/(MaxValue - MinValue));
    xBottom := Self.Height * ((SourceCandel.Low - MinValue)/(MaxValue - MinValue));
    Candel.SetBounds(0,xTop,Self.Width,Self.Height - (xTop + xBottom));
    xTopOpen := 0;
    xBottomClose := 0;

    Candel.Body.Fill.Color := TAlphaColorRec.Black;
    if (SourceCandel.Open > SourceCandel.Close) then
    begin
      xTopOpen := Candel.Height * ((SourceCandel.High - SourceCandel.Open)/(SourceCandel.High - SourceCandel.Low));
      xBottomClose := Candel.Height * ((SourceCandel.Close - SourceCandel.Low)/(SourceCandel.High - SourceCandel.Low));
      Candel.Body.Fill.Color := TAlphaColorRec.Red;
    end
    else if (SourceCandel.Open < SourceCandel.Close) then
    begin
      xTopOpen := Candel.Height * ((SourceCandel.High - SourceCandel.Close)/(SourceCandel.High - SourceCandel.Low));
      xBottomClose := Candel.Height * ((SourceCandel.Open - SourceCandel.Low)/(SourceCandel.High - SourceCandel.Low));
      Candel.Body.Fill.Color := TAlphaColorRec.Green;
    end;
    Candel.LayoutHigh.Height := xTopOpen;
    Candel.LayoutLow.Height := xBottomClose;
    Candel.Resize;
  end;
end;

{ TChartsFrame }

class function TChartsFrame.GetCreateFrame(const AParent: TFmxObject): TChartsFrame;
begin
  if not Assigned(localChartsFrame) then
  begin
    localChartsFrame := TChartsFrame.Create(nil);
    localChartsFrame.Align := TAlignLayout.Client;
  end;
  localChartsFrame.Parent := AParent;
  Result := localChartsFrame;
end;

class function TChartsFrame.GetSource: TSourceCandel;
begin
  if Assigned(localChartsFrame) then
    Result := localChartsFrame.Source
  else
    raise Exception.Create('Error Message: Фраме: TChartsFrame (не инициализирован)');
end;


class procedure TChartsFrame.SetShowCharts(const ACount: Integer);
begin
  if Assigned(localChartsFrame) then
  begin
    localChartsFrame.SetCreateCandels(ACount);
    localChartsFrame.SetShowCandels;
  end;
end;

class procedure TChartsFrame.SetShowCharts(const ABeginIndex, ACount: Integer);
begin
  if Assigned(localChartsFrame) then
  begin
    localChartsFrame.SetCreateCandels(ABeginIndex,ACount);
    localChartsFrame.SetShowCandels;
  end;
end;

procedure TChartsFrame.LayoutResize(Sender: TObject);
begin
  Self.SetShowCandels;
end;

procedure TChartsFrame.SetCreateCandels(const ACount: Integer);
begin
  Self.SetCreateCandels(-1,ACount);
end;

procedure TChartsFrame.SetCreateCandels(const ABeginIndex, ACount: Integer);
var
  xCandel: Lb.Candel.SysUtils.TCandel;
  xMaxValue, xMinValue: Double;
  xCandelLayout: TChartsFrame.TCandelLayout;
var
  xLow, xHigh: Integer;
begin
  if (ACount > 0) and (FSource.Candels.Count > 0) and (ACount < FSource.Candels.Count) then
  begin
    FSource.GetMaxAndMinValue(ABeginIndex,ACount,xMaxValue,xMinValue);

    if ABeginIndex >= 0 then
    begin
      xLow := ABeginIndex;
      xHigh := xLow + ACount;
      if xHigh >= FSource.Candels.Count then
         xHigh := FSource.Candels.Count - 1;
    end
    else
    begin
      if ACount > 0 then
      begin
        xLow := FSource.Candels.Count - ACount;
        xHigh := FSource.Candels.Count - 1;
      end
      else
      begin
        xLow := 0;
        xHigh := FSource.Candels.Count - 1;
      end;
    end;


    var xIndex := -1;
    ClearCandels;
    for var i := xLow to xHigh do
    begin
      Inc(xIndex);
      xCandelLayout := Self.GetCreateCandelLayout(xIndex);
      xCandelLayout.MaxValue := xMaxValue;
      xCandelLayout.MinValue := xMinValue;

      xCandel := FSource.Candels[i];
      xCandelLayout.SourceCandel := xCandel;
      xCandelLayout.SetPositionCandel;

    end;
  end;
end;

procedure TChartsFrame.SetShowCandels;
begin
  try
    if not Assigned(FCalbelLayouts) then
      Exit;
    var Count := FCalbelLayouts.Count;
    if Count > 0 then
    begin
      var xDelta := Layout.Width / Count;
      for var i := 0 to Count - 1 do
      begin
        var xCandelLayout := FCalbelLayouts[i];
        xCandelLayout.SetBounds(i * xDelta,0,xDelta,Layout.Height);
        xCandelLayout.SetPositionCandel;
      end;
    end;
  except
  end;
end;

class procedure TChartsFrame.SetFreeAndNil;
begin
  if Assigned(localChartsFrame) then
    FreeAndNil(localChartsFrame);
  localChartsFrame := nil;
end;

constructor TChartsFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FSource := TSourceCandel.Create;
  FCalbelLayouts := TCalbelLayoutList.Create;
end;

destructor TChartsFrame.Destroy;
begin
  FreeAndNil(FCalbelLayouts);
  FreeAndNil(FSource);
  inherited;
end;


procedure TChartsFrame.ClearCandels;
begin
  for var xCandelLayout in FCalbelLayouts do
    xCandelLayout.Visible := False;
end;

function TChartsFrame.GetCreateCandelLayout(const AIndex: Integer): TChartsFrame.TCandelLayout;
begin
  if AIndex >= FCalbelLayouts.Count then
  begin
    var xCandelLayot := TChartsFrame.TCandelLayout.Create(Self);
    FCalbelLayouts.Add(xCandelLayot);
    xCandelLayot.Parent := Layout;
    Result := xCandelLayot;
  end
  else
  begin
    var xCandelLayot := FCalbelLayouts[AIndex];
    xCandelLayot.Visible := True;
    Result := xCandelLayot;
  end;
end;

initialization

finalization

end.
