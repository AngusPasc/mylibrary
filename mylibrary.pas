unit MyLibrary;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db, FileUtil, Forms, Controls,
  Graphics, Dialogs, DBGrids, StdCtrls, DbCtrls, Grids, UMBAuthor, UMBComposition, UMBEditor,
  UMBGenre, UMBPublisher, UMBTranslator, UMBBook;

type

  { TMyBooks }

  TMyBooks = class(TForm)
    btnBooks: TButton;
    btnAuthors: TButton;
    btnGenres: TButton;
    dtnAddBook: TButton;
    DataSource1: TDataSource;
    edtBookName: TEdit;
    edtBookISBN: TEdit;
    edtOrigName: TEdit;
    edtBookYear: TEdit;
    edtBookAnnotation: TEdit;
    Label1: TLabel;
    Label2: TLabel;
    Label3: TLabel;
    Label4: TLabel;
    Label5: TLabel;
    SQLite3Connection1: TSQLite3Connection;
    SQLQuery1: TSQLQuery;
    SQLTransaction1: TSQLTransaction;
    StringGrid1: TStringGrid;
    procedure btnAuthorsClick(Sender: TObject);
    procedure btnGenresClick(Sender: TObject);
    procedure btnBooksClick(Sender: TObject);
    procedure dtnAddBookClick(Sender: TObject);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure DeleteRowsFromStringGrid();
  private
    { private declarations }
  public
    { public declarations }
  end;

var
  MyBooks: TMyBooks;

implementation

{$R *.lfm}

{ TMyBooks }

procedure TMyBooks.FormCreate(Sender: TObject);
begin
  //SQLQuery1.Close;
end;

//ДК, очищаем таблицу, оставляем только строку с заголовками
procedure TMyBooks.DeleteRowsFromStringGrid();
var
  I : Integer;
begin
     StringGrid1.Clean([gzNormal]);


     for I:=StringGrid1.RowCount downto 1 do;
         StringGrid1.RowCount:=StringGrid1.RowCount-1;

end;

procedure TMyBooks.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
     SQLQuery1.Close;
     SQLTransaction1.Active:=False;
     SQLite3Connection1.Connected:=False;
end;

procedure TMyBooks.btnBooksClick(Sender: TObject);
var
  I : Integer;
  c: TGridColumn;
  ColumNNames : array[0..4] of string = ('Название', 'ISBN', 'Оригинал', 'Год изд.', 'Аннотация');
begin
     DeleteRowsFromStringGrid();

     for I:=StringGrid1.RowCount downto 1 do;
         StringGrid1.RowCount:=StringGrid1.RowCount-1;

     for I:= 0 to Length(ColumnNames)-1 do
     begin
          c := StringGrid1.Columns.Add;
          c.Title.Caption:=ColumnNames[I];
     end;
     //ДК, обработка запроса, пытаемся вывести результат
     I:=1;
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT name, isbn, orig_name, year, note FROM books';
     SQLQuery1.Open;
     while not SQLQuery1.EOF do
     begin

        StringGrid1.RowCount:=StringGrid1.RowCount + 1;
        StringGrid1.InsertRowWithValues(I, [IntToStr(I), SQLQuery1.FieldByName('name').AsString,
                                        SQLQuery1.FieldByName('isbn').AsString, SQLQuery1.FieldByName('orig_name').AsString,
                                        SQLQuery1.FieldByName('year').AsString, SQLQuery1.FieldByName('note').AsString]);
        I:=I+1;
        SQLQuery1.Next;
     end;
     SQLQuery1.Close;
end;

procedure TMyBooks.dtnAddBookClick(Sender: TObject);
begin
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='INSERT INTO books (name, isbn, orig_name, year, note) VALUES (:bNAME, :bISBN, :bORIGNAME, :bYEAR, :bNOTE)';
     SQLQuery1.Params.ParamByName('bNAME').AsString:=edtBookName.Text;
     SQLQuery1.Params.ParamByName('bISBN').AsString:=edtBookISBN.Text;
     SQLQuery1.Params.ParamByName('bORIGNAME').AsString:=edtOrigName.Text;
     SQLQuery1.Params.ParamByName('bYEAR').AsString:=edtBookYear.Text;
     SQLQuery1.Params.ParamByName('bNOTE').AsString:=edtBookAnnotation.Text;
     SQLQuery1.ExecSQL;
     SQLTransaction1.Commit;
end;

procedure TMyBooks.btnGenresClick(Sender: TObject);
begin

end;

procedure TMyBooks.btnAuthorsClick(Sender: TObject);
var
  I : Integer;
  c: TGridColumn;
  ColumNNames : array[0..1] of string = ('Фамилия', 'Имя');
begin
      StringGrid1.Clean([gzNormal]);

     for I:=StringGrid1.RowCount downto 1 do;
         StringGrid1.RowCount:=StringGrid1.RowCount-1;

     for I:= 0 to Length(ColumnNames)-1 do
     begin
          c := StringGrid1.Columns.Add;
          c.Title.Caption:=ColumnNames[I];
     end;
     //ДК, обработка запроса, пытаемся вывести результат
     I:=1;
     SQLQuery1.Close;
     SQLQuery1.SQL.Text:='SELECT surname, name FROM authors';
     SQLQuery1.Open;
     while not SQLQuery1.EOF do
     begin

        StringGrid1.RowCount:=StringGrid1.RowCount + 1;
        StringGrid1.InsertRowWithValues(I, [IntToStr(I), SQLQuery1.FieldByName('surname').AsString,
                                        SQLQuery1.FieldByName('name').AsString]);
        I:=I+1;
        SQLQuery1.Next;
     end;
     SQLQuery1.Close;
end;

end.

