import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <form>
                <input
                    type="checkbox"
                    onChange={this.props.onToggleShowChords}
                    checked={this.props.showChords}
                />
            </form>
        );
    }

});


