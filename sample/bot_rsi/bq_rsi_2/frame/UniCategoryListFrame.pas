unit UniCategoryListFrame;

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
  FMX.Layouts,
  UnitCategoryFrame,
  Lb.Criteria,
  Lb.SysUtils;

type
  ///<summary>Заявка</summary>
  TCategoryListFrame = class(TFrame)
    GridPanel: TGridPanelLayout;
    Layout1: TLayout;
    Layout2: TLayout;
    Layout3: TLayout;
    Layout4: TLayout;
    Layout5: TLayout;
  private
    FManagerCriteria: TManagerCriteria;
    procedure InitCategoryFrame;
    procedure SetManagerCriteria(const Value: TManagerCriteria);
    function GetSide: TTypeBuySell;
  protected
    Category1: TCategoryFrame;
    Category2: TCategoryFrame;
    Category3: TCategoryFrame;
    Category4: TCategoryFrame;
    Category5: TCategoryFrame;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    property Side: TTypeBuySell read GetSide;
    property ManagerCriteria: TManagerCriteria read FManagerCriteria write SetManagerCriteria;
  end;

implementation

uses
  Lb.Logger;

{$R *.fmx}

{ TOrderCategoryFrame }

constructor TCategoryListFrame.Create(AOwner: TComponent);
begin
  inherited;
  InitCategoryFrame;
end;

destructor TCategoryListFrame.Destroy;
begin
  FreeAndNil(Category1);
  FreeAndNil(Category2);
  FreeAndNil(Category3);
  FreeAndNil(Category4);
  FreeAndNil(Category5);
  inherited;
end;

procedure TCategoryListFrame.InitCategoryFrame;

  function _InitCategoryFrame(const ALayout: TLayout): TCategoryFrame;
  var
    xFrame: TCategoryFrame;
  begin
    xFrame := TCategoryFrame.Create(nil);
    xFrame.Parent := ALayout;
    xFrame.Align := TAlignLayout.Client;
    Result := xFrame;
  end;

begin
  Category1 := _InitCategoryFrame(Layout1);
  Category2 := _InitCategoryFrame(Layout2);
  Category3 := _InitCategoryFrame(Layout3);
  Category4 := _InitCategoryFrame(Layout4);
  Category5 := _InitCategoryFrame(Layout5);
end;

function TCategoryListFrame.GetSide: TTypeBuySell;
begin
  if Assigned(FManagerCriteria) then
    Result := FManagerCriteria.Side
  else
    Result := TTypeBuySell.tsNull;
end;

procedure TCategoryListFrame.SetManagerCriteria(const Value: TManagerCriteria);

  function _IsCriteria(AIndex: Integer): Boolean;
  begin
    Result := (FManagerCriteria.Count - 1) >= AIndex;
  end;

begin
  FManagerCriteria := Value;
  case FManagerCriteria.Side of
    tsBuy: begin
      Category1.Criteria := FManagerCriteria.Items[0];
      Category2.Criteria := FManagerCriteria.Items[1];
      Category3.Criteria := FManagerCriteria.Items[2];
      Category4.Criteria := FManagerCriteria.Items[3];
      Category5.Criteria := FManagerCriteria.Items[4];
    end;
    tsSell: begin
      Category5.Criteria := FManagerCriteria.Items[0];
      Category4.Criteria := FManagerCriteria.Items[1];
      Category3.Criteria := FManagerCriteria.Items[2];
      Category2.Criteria := FManagerCriteria.Items[3];
      Category1.Criteria := FManagerCriteria.Items[4];
    end;
  end;
end;

end.
