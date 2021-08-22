unit UnitParamVectorFrame;

interface

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
  System.Rtti,
  FMX.Grid.Style,
  FMX.Objects,
  FMX.Controls.Presentation,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Layouts,
  Lb.ChartsFrame,
  Lb.Candel.Blocks;

type
  ///<summary>Здесь с равниваем два вектора</summary>
  ///<remarks>Поиск оптимального с равнение</remarks>
  TParamVectorFrame = class(TFrame)
    RectangleLeft: TRectangle;
    GridPanelLayout1: TGridPanelLayout;
    VectorLeftLayout: TLayout;
    VectorRightLayout: TLayout;
    RectangleRigth: TRectangle;
  private
    FLeftChart: TChartsFrame;
    FRigthChart: TChartsFrame;
  protected
    property LeftChart: TChartsFrame read FLeftChart;
    property RightChart: TChartsFrame read FRigthChart;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure SetShowBlock(const ABlock1, ABlock2: TBlock);
  end;

implementation

{$R *.fmx}

{$DEFINE DB_LOG}

uses
  Lb.Logger;

{ TParamVectorFrame }

constructor TParamVectorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FLeftChart := TChartsFrame.Create(nil);
  FLeftChart.Align := TAlignLayout.Client;
  FLeftChart.Parent := RectangleLeft;


  FRigthChart := TChartsFrame.Create(nil);
  FRigthChart.Align := TAlignLayout.Client;
  FRigthChart.Parent := RectangleRigth;

end;

destructor TParamVectorFrame.Destroy;
begin
  FreeAndNil(FRigthChart);
  FreeAndNil(FLeftChart);
  inherited;
end;

procedure TParamVectorFrame.SetShowBlock(const ABlock1, ABlock2: TBlock);
begin
  FLeftChart.Source.Assign(ABlock1.Vectors);
  FLeftChart.SetCreateCandels(16);
  FLeftChart.SetShowCandels;

  FRigthChart.Source.Assign(ABlock2.Vectors);
  FRigthChart.SetCreateCandels(16);
  FRigthChart.SetShowCandels;

end;

end.
