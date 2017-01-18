unit UMBBook;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, sqlite3conn, sqldb, db;

type
  TMBBook = Class
    private
      FBookID : Integer;
      FBookName : String;
      FBookISBN : String;
      FBookYear : String;
      FBookNote : String;
      FBookCompositions : array of Integer;
      FBookAuthors : array of Integer;
      FBookPublisherID : Integer;
      FBookEditorID : Integer;
      FBookGenreID : Integer;
      FNewBook : Boolean;
      //
      procedure GetBookCompositions(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      function GetBookCompositionID( Index : Integer ) : Integer;
      //procedure GetBookAuthors(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      function GetBookAuthorID( Index : Integer ) : Integer;
      //procedure GetBookPublisherID(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      //procedure GetBookEditor(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      //procedure GetBookGenres();
      procedure SetBookName( Name : String );
      procedure SetBookISBN( ISBN : String );
      procedure SetBookYear( Year : String );
      procedure SetBookNote( Note : String );
      //
      procedure ReadBookAuthorsFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookEditorFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookGenresFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookPublisherFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      procedure ReadBookDataFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
    public
      constructor Create(); overload;
      constructor Create(BookName : String; BookISBN : String; BookYear : String; BookNote : String); overload;
      constructor Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction); overload;
      procedure LoadBookByID(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
      property BookID : Integer read FBookID;
      property BookName : String read FBookName write SetBookName;
      property BookISBN : String read FBookISBN write SetBookISBN;
      property BookYear : String read FBookYear write SetBookYear;
      property BookNote : String read FBookNote write SetBookNote;
      property BookComposition[Index : Integer] : Integer read GetBookCompositionID;
      property BookAuthor[Index : Integer] : Integer read GetBookAuthorID;

    published
  end;


implementation
uses
  Dialogs;

constructor TMBBook.Create();
begin
     inherited;
     FBookName := '';
     FBookISBN := '';
     FBookYear := '';
     FBookNote := '';
     FNewBook := False;
end;

constructor TMBBook.Create(BookName : String; BookISBN : String; BookYear : String; BookNote : String);
begin
     FBookName := BookName;
     FBookISBN := BookISBN;
     FBookYear := BookYear;
     FBookNote := BookNote;
     FNewBook := True;
end;

constructor TMBBook.Create(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, isbn, year, note FROM books where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     //ДК, нужна проверка что запрос вернул не пустой результат
     FBookName:=SQLQuery.FieldByName('name').AsString;
     FBookISBN:=SQLQuery.FieldByName('isbn').AsString;
     FBookYear:=SQLQuery.FieldByName('year').AsString;
     FBookNote:=SQLQuery.FieldByName('note').AsString;
     FBookID:=ID;
     SQLQuery.Close;
     ReadBookDataFromDB(SQLQuery, SQLTransaction);
     FNewBook := False;

end;

procedure TMBBook.LoadBookByID(ID : Integer; const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT name, isbn, year, note FROM books where id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(ID);
     SQLQuery.Open;
     ShowMessage(SQLQuery.FieldByName('name').AsString);
     //ДК, нужна проверка что запрос вернул не пустой результат
     FBookName:=SQLQuery.FieldByName('name').AsString;
     FBookISBN:=SQLQuery.FieldByName('isbn').AsString;
     FBookYear:=SQLQuery.FieldByName('year').AsString;
     FBookNote:=SQLQuery.FieldByName('note').AsString;
     FBookID:=ID;
     SQLQuery.Close;
     ReadBookDataFromDB(SQLQuery, SQLTransaction);
     FNewBook := False;

end;

procedure TMBBook.ReadBookAuthorsFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
var
  I, J : Integer;
begin
     if Length(FBookCompositions) > 0 then
     begin
          I:=0;
          for J:=0 to Length(FBookCompositions) do
          begin
               SQLQuery.Close;
               SQLQuery.SQL.Text:='SELECT author_id FROM rel_composition_authors where composition_id=:bID';
               SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookCompositions[J]);
               SQLQuery.Open;
               SQLQuery.Last;
               if SQLQuery.RecordCount > 0 then
               begin
                    SetLength(FBookAuthors, SQLQuery.RecordCount+Length(FBookAuthors));
                    SQLQuery.First;
                    while not SQLQuery.EOF do
                    begin
                         FBookAuthors[I]:=SQLQuery.FieldByName('author_id').AsInteger;
                         I:=I+1;
                         SQLQuery.Next;
                    end;
                    SQLQuery.Close;
               end;
          end;
     end;
end;

procedure TMBBook.ReadBookEditorFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT editor_id FROM rel_book_editors where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     SQLQuery.Last;
     FBookEditorID:=SQLQuery.FieldByName('editor_id').AsInteger;
end;

procedure TMBBook.ReadBookPublisherFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT publisher_id FROM rel_publisher_books where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     SQLQuery.Last;
     FBookPublisherID:=SQLQuery.FieldByName('publisher_id').AsInteger;
end;

procedure TMBBook.ReadBookGenresFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT genre_id FROM rel_books_genres where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     SQLQuery.Last;
     FBookGenreID:=SQLQuery.FieldByName('genre_id').AsInteger;
end;

procedure TMBBook.ReadBookDataFromDB(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
begin
     GetBookCompositions(SQLQuery, SQLTransaction);
     ReadBookAuthorsFromDB(SQLQuery, SQLTransaction);
     ReadBookEditorFromDB(SQLQuery, SQLTransaction);
     ReadBookPublisherFromDB(SQLQuery, SQLTransaction);
     ReadBookGenresFromDB(SQLQuery, SQLTransaction);
end;

procedure TMBBook.GetBookCompositions(const SQLQuery :  TSQLQuery; const SQLTransaction : TSQLTransaction);
var
  I : Integer;
begin
     SQLQuery.Close;
     SQLQuery.SQL.Text:='SELECT composition_id FROM rel_book_compositions where book_id=:bID';
     SQLQuery.Params.ParamByName('bID').AsString:=IntToStr(FBookID);
     SQLQuery.Open;
     SQLQuery.Last;
     if SQLQuery.RecordCount > 0 then
     begin
          SetLength(FBookCompositions, SQLQuery.RecordCount);
          SQLQuery.First;
          I:=0;
          while not SQLQuery.EOF do
          begin
               FBookCompositions[I]:=SQLQuery.FieldByName('composition_id').AsInteger;
               I:=I+1;
               SQLQuery.Next;
          end;
          SQLQuery.Close;
     end;
end;

function TMBBook.GetBookCompositionID( Index : Integer ) : Integer;
begin
     Result := FBookCompositions[Index];
end;

function TMBBook.GetBookAuthorID( Index : Integer ) : Integer;
begin
     Result := FBookAuthors[Index];
end;

procedure TMBBook.SetBookName( Name : String );
begin
     FBookName := Name;
end;

procedure TMBBook.SetBookISBN( ISBN : String );
begin
     FBookISBN := ISBN;
end;

procedure TMBBook.SetBookYear( Year : String );
begin
     FBookYear := Year;
end;

procedure TMBBook.SetBookNote( Note : String );
begin
     FBookNote := Note;
end;

end.

