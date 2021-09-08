unit Lb.IndexTableFrame;

interface

uses
  System.SysUtils,
  System.Types,
  System.UITypes,
  System.Classes,
  System.Variants,
  FMX.Types,
  FMX.Graphics,
  FMX.Controls,
  FMX.Forms,
  FMX.Dialogs,
  FMX.StdCtrls,
  System.Rtti,
  FMX.Grid.Style,
  FMX.ScrollBox,
  FMX.Grid,
  FMX.Controls.Presentation,
  FMX.Layouts,
  Lb.Create.DB;

type
  ///<summary>Список индексов</summary>
  TIndexTableFrame = class(TFrame)
    Layout: TLayout;
    LayoutMenu: TLayout;
    ButtonAdd: TButton;
    ButtonEdit: TButton;
    ButtonDelete: TButton;
    StringGrid: TStringGrid;
  private
    FIndexs: TCrIndexs;
    procedure SetIndexs(const Value: TCrIndexs);
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Indexs: TCrIndexs read FIndexs write SetIndexs;
  end;

implementation

{$R *.fmx}

{ TIndexTableFrame }

constructor TIndexTableFrame.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);
  FIndexs := nil;
end;

destructor TIndexTableFrame.Destroy;
begin

  inherited;
end;

procedure TIndexTableFrame.SetIndexs(const Value: TCrIndexs);
begin
  FIndexs := Value;
end;

end.
