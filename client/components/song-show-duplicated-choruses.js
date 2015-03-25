import React from 'react';

export default React.createClass({

    render: function () {
        return (
            <form>
                <input
                    type="checkbox"
                    onChange={this.props.onToggleShowDuplicatedChoruses}
                    checked={this.props.showDuplicatedChoruses}
                />
            </form>
        );
    }

});

