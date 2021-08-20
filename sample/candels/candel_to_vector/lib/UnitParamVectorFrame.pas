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
  FMX.Layouts;

type
  ///<summary>Здесь с равниваем два вектора</summary>
  ///<remarks>Поиск оптимального с равнение</remarks>
  TParamVectorFrame = class(TFrame)
    Rectangle: TRectangle;
    GridPanelLayout1: TGridPanelLayout;
    VectorLeftLayout: TLayout;
    VectorRightLayout: TLayout;
  private

  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
  end;

implementation

{$R *.fmx}

{ TParamVectorFrame }

constructor TParamVectorFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

end;

destructor TParamVectorFrame.Destroy;
begin

  inherited;
end;



end.
