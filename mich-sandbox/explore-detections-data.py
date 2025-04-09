import marimo

__generated_with = "0.12.4"
app = marimo.App(width="medium")


@app.cell
def _():
    import marimo as mo
    import pandas as pd
    import h5py
    return h5py, mo, pd


@app.cell(hide_code=True)
def _(mo):
    selected_file = mo.ui.file_browser("mich-sandbox/",
                                      filetypes=[".h5"])
    selected_file
    return (selected_file,)


@app.cell(hide_code=True)
def _(h5py, selected_file):
    with h5py.File(selected_file.path(), "r") as f:
        detector_types = list(f.keys())
    return detector_types, f


@app.cell(hide_code=True)
def _(detector_types, mo):
    det_type_pick = mo.ui.dropdown(detector_types, value=detector_types[0])
    mo.vstack([
        mo.md("Select a detector to inspect:"),
        det_type_pick
    ])
    return (det_type_pick,)


app._unparsable_cell(
    r"""
    click_df = pd.read_hdf(selected_file.path(), key=det_type_pick.value)

    click_df['UTC'] = pd.to_datetime(click_df['UTC'], unit='s', errors='coerce')
    click_df['UTC'] = click_df['UTC'].dt.ro˙und('ms')

    click_df

    """,
    column=None, disabled=False, hide_code=True, name="_"
)


@app.cell(hide_code=True)
def _(click_df, mo):
    mo.hstack([
        mo.md("**Table length:** "),
        len(click_df)
    ], 
              justify="start")

    return


@app.cell
def _():
    return


@app.cell
def _():
    return


if __name__ == "__main__":
    app.run()
